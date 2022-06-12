package sttp.model

import sttp.model.internal.{ArrayView, ParseUtils, Singleton, Rfc3986}
import java.util.{ArrayList, List => JList}

import sttp.model.internal.{ParseUtils, Rfc3986}

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

trait UriInterpolator {
  implicit class UriContext(val sc: StringContext) {

    /** Parse the given string (with embedded expressions) as an uri.
      *
      * Any values embedded in the URI using the `${...}` syntax will be URL-encoded, taking into account the context.
      * Parts of the URI given as literal strings (not embedded values), are assumed to be URL-encoded and thus will be
      * decoded when creating the `Uri` instance.
      *
      * Embedded values can be optional for hosts (subdomains) query parameters and the fragment. If the value is
      * `None`, the appropriate URI component will be removed.
      *
      * Sequences in the host part will be expanded to a subdomain sequence, and sequences in the path will be expanded
      * to path components. Maps, sequences of tuples and sequences of values can be embedded in the query part. They
      * will be expanded into query parameters. Maps and sequences of tuples can also contain optional values, for which
      * mappings will be removed if `None`.
      *
      * All components of the URI can be embedded from values: scheme, username/password, host, port, path, query and
      * fragment. The embedded values won't be further parsed, with the exception of the `:` in the host part, which is
      * commonly used to pass in both the host and port.
      *
      * If a string containing the protocol is embedded at the very beginning, it will not be escaped, allowing to embed
      * entire addresses as prefixes, e.g.: `uri"$endpoint/login"`, where `val endpoint = "http://example.com/api"`.
      * This is useful when a base URI is stored in a value, and can then be used as a base for constructing more
      * specific URIs.
      *
      * @throws IllegalArgumentException
      *   In case of a validation error. For a safe version, see [[Uri.parse()]].
      */
    def uri(args: Any*): Uri = UriInterpolator.interpolate(sc, args: _*)
  }
}

object UriInterpolator {
  def interpolate(sc: StringContext, args: Any*): Uri = {
    val tokens = tokenize(sc, args: _*)

    val builders = List(
      UriBuilder.Scheme,
      UriBuilder.UserInfo,
      UriBuilder.HostPort,
      UriBuilder.Path,
      UriBuilder.Query,
      UriBuilder.Fragment
    )

    val startingUri = Uri(None, None, Uri.EmptyPath, Nil, None)

    val (uri, leftTokens) =
      builders.foldLeft((startingUri, filterNulls(tokens))) { case ((u, t), builder) =>
        builder.fromTokens(u, t)
      }

    if (leftTokens.nonEmpty) {
      throw new IllegalStateException(s"Tokens left after building the whole uri: $leftTokens, result so far: $uri")
    }

    uri
  }

  private def tokenize(sc: StringContext, args: Any*): JList[Token] = {
    val strings = sc.parts.iterator
    val expressions = args.iterator

    val tokens = new ArrayList[Token]()
    var tokenizer = Tokenizer.Scheme.tokenize(tokens, strings.next())

    while (strings.hasNext) {
      val nextExpression = expressions.next()
      val nextExpressionStr = nextExpression.toString

      val nextStr = strings.next()

      // Special case: the interpolation starts with an expression, which contains a whole URI. This URI can be
      // absolute - in which case the expression should contain ://; or relative - in which case, the next string
      // token can't be the scheme separator ://.
      //
      // Parsing the expression as if its string value was embedded in the interpolated string. This way it's possible
      // to extend existing URIs. Without special-casing the embedded URI would be escaped and become part of the host
      // as a whole.
      if (tokens.size() == 1 && tokens.get(0) == StringToken("") && (nextExpressionStr.contains("://") || !nextStr.contains("://"))) {
        def tokenizeExpressionAsString(): Unit =
          tokenizer = tokenizer.tokenize(tokens, nextExpression.toString)

        def tokenizeStringRemoveEmptyPrefix(): Unit = {
          val initLength = tokens.size()
          tokenizer = tokenizer.tokenize(tokens, nextStr)

          // we need to remove empty tokens around exp as well - however here
          // by hand, as the expression token is unwrapped, so removeEmptyTokensAroundExp
          // won't handle this.
          if (initLength < tokens.size() && tokens.get(initLength) == StringToken("")) {
            tokens.set(initLength, null)
          }

          def isSlash(t: Token) = t == SlashInPath || t == PathStart

          // remove trailing slash when path is added to an interpolated uri:
          // val a = uri"http://example.com/" // notice the trailing slash
          // val b = uri"$a/xy" // "http://example.com/xy"
          for {
            (slashInPathIdx, _) <- nextNonNull(tokens, initLength).filter(_._2 == SlashInPath)
            (emptyStringTokenIdx, _) <- lastNonNull(tokens, initLength - 1).filter(_._2 == StringToken(""))
            _ <- lastNonNull(tokens, emptyStringTokenIdx - 1).filter(tuple => isSlash(tuple._2))
          } {
            tokens.set(slashInPathIdx, null)
            tokens.set(emptyStringTokenIdx, null)
          }
        }

        tokenizeExpressionAsString()
        tokenizeStringRemoveEmptyPrefix()
      } else {
        tokens.add(ExpressionToken(nextExpression))

        tokenizer = tokenizer.tokenize(tokens, nextStr)
      }
    }

    tokenizer.endToken.foreach(tokens.add)
    removeEmptyTokensAroundExp(tokens)
    addPathStartAfterAuthorityOrSchemeEnd(tokens)
    tokens
  }

  sealed trait Token
  case class StringToken(s: String) extends Token
  case class ExpressionToken(e: Any) extends Token
  case object SchemeEnd extends Token
  case object ColonInAuthority extends Token
  case object AtInAuthority extends Token
  case object DotInAuthority extends Token
  case object AuthorityEnd extends Token
  case object PathStart extends Token
  case object SlashInPath extends Token
  case object QueryStart extends Token
  case object AmpInQuery extends Token
  case object EqInQuery extends Token
  case object FragmentStart extends Token

  trait Tokenizer {
    def tokenize(list: JList[Token], s: String): Tokenizer
    def endToken: Option[Token] = None // token to add if the input is exhausted
  }

  object Tokenizer {
    private val AuthorityTerminators = Set('/', '?', '#')

    object Scheme extends Tokenizer {
      private val SchemePattern = "[A-Za-z][A-Za-z0-9+.-]*".r

      override def tokenize(list: JList[Token], s: String): Tokenizer = {
        SchemePattern.findPrefixOf(s) match {
          // #59: if the entire string matches the pattern, then there's no scheme terminator (`:`). This means there's
          // no scheme, hence - tokenizing as a relative uri.
          case Some(scheme) if scheme.length == s.length => AfterScheme.tokenize(list, scheme)
          case _ if s.isEmpty =>
            // scheme (or another component) might be continued
            list.add(StringToken(""))
            this
          case Some(scheme) if s(scheme.length) == ':' =>
            val rest = s.substring(scheme.length + 1)
            list.add(StringToken(scheme))
            list.add(SchemeEnd)
            AfterScheme.tokenize(list, rest)
          case _ if s.startsWith(":") => // there was an expression token before, end of scheme
            list.add(SchemeEnd)
            AfterScheme.tokenize(list, s.substring(1))
          case _ => // no scheme
            AfterScheme.tokenize(list, s)
        }
      }

      override def endToken: Option[Token] = Some(SchemeEnd)
    }

    object AfterScheme extends Tokenizer {
      override def tokenize(list: JList[Token], s: String): Tokenizer = {
        if (s == "") {
          list.add(StringToken(""))
          this
        } else if (s.startsWith("//")) {
          Authority.tokenize(list, s.substring(2)) // uri with authority
        } else { // uri without authority
          val first = s(0)
          if (AuthorityTerminators.contains(first)) {
            val (tokenizer, token) = separatorTokenizerAndToken(first)
            if (token == PathStart) {
              // absolute path in a relative uri, adding an empty string token so that the absolute path is preserved
              // (might be a continuation if there was no scheme)
              list.add(StringToken(""))
              list.add(SlashInPath)
            } else {
              list.add(token)
            }
            tokenizer.tokenize(list, s.substring(1))
          } else {
            // non-slash-initiated path (might be a continuation if there was no scheme)
            Path.tokenize(list, s)
          }
        }
      }
    }

    object Authority extends Tokenizer {
      private val IpV6InAuthorityPattern = "\\[[0-9a-fA-F:]+\\]".r // see the pattern in Uri.HostEncoding

      override def tokenize(list: JList[Token], s: String): Tokenizer = {
        val initSize = list.size()
        val tokenizer = tokenizeTerminatedFragment(
          s,
          this,
          list,
          Set('/', '?', '#'),
          Map(':' -> ColonInAuthority, '@' -> AtInAuthority, '.' -> DotInAuthority),
          Some(('[', ']'))
        )
        (initSize until list.size()).foreach { idx =>
          list.get(idx) match {
            case StringToken(s @ IpV6InAuthorityPattern()) =>
              list.set(idx, StringToken(s.substring(1, s.length() - 1)))
            case _ => ()
          }
        }
        tokenizer
      }

      override def endToken: Option[Token] = Some(AuthorityEnd)
    }

    object Path extends Tokenizer {
      override def tokenize(list: JList[Token], s: String): Tokenizer =
        tokenizeTerminatedFragment(
          s,
          this,
          list,
          Set('?', '#'),
          Map('/' -> SlashInPath)
        )
    }

    object Query extends Tokenizer {
      override def tokenize(list: JList[Token], s: String): Tokenizer =
        tokenizeTerminatedFragment(
          s,
          this,
          list,
          Set('#'),
          Map('&' -> AmpInQuery, '=' -> EqInQuery)
        )
    }

    object Fragment extends Tokenizer {
      override def tokenize(list: JList[Token], s: String): Tokenizer = {
        list.add(StringToken(s))
        this
      }
    }

    /** Tokenize the given string up to any of the given terminator characters by splitting it using the given
      * separators and translating each separator to a token.
      *
      * The rest of the string, after the terminators, is tokenized using a tokenizer determined by the type of the
      * terminator.
      *
      * @param separatorsEscape
      *   A context-specific pair of escape characters (start/stop), in which separators are not taken into account.
      */
    private def tokenizeTerminatedFragment(
        s: String,
        current: Tokenizer,
        list: JList[Token],
        terminators: Set[Char],
        separatorsToTokens: Map[Char, Token],
        separatorsEscape: Option[(Char, Char)] = None
    ): Tokenizer = {
      def tokenizeFragment(f: String): Unit = {
        val initLength = list.size()
        splitPreserveSeparators(list, f, separatorsToTokens.keySet, separatorsEscape)
        (initLength until list.size()).foreach { idx =>
          list.get(idx) match {
            case StringToken(s) =>
              s.headOption.flatMap(separatorsToTokens.get).foreach(list.set(idx, _))
            case _ => ()
          }
        }
      }

      // first checking if the fragment doesn't end; e.g. the authority is
      // terminated by /, ?, # or end of string (there might be other /, ?,
      // # later on e.g. in the query).
      // See: https://tools.ietf.org/html/rfc3986#section-3.2
      split(s, terminators, None) match {
        case Right((fragment, separator, rest)) =>
          tokenizeFragment(fragment)
          current.endToken.foreach(list.add)
          tokenizeAfterSeparator(list, separator, rest)

        case Left(fragment) =>
          tokenizeFragment(fragment)
          current
      }
    }

    private def tokenizeAfterSeparator(
        acc: JList[Token],
        separator: Char,
        s: String
    ): Tokenizer = {
      val (next, separatorToken) = separatorTokenizerAndToken(separator)
      acc.add(separatorToken)
      next.tokenize(acc, s)
    }

    private def separatorTokenizerAndToken(separator: Char): (Tokenizer, Token) =
      separator match {
        case '/' => (Path, PathStart)
        case '?' => (Query, QueryStart)
        case '#' => (Fragment, FragmentStart)
      }

    private def splitPreserveSeparators(acc: JList[Token], s: String, sep: Set[Char], escape: Option[(Char, Char)]): Unit = {
      @tailrec
      def doSplit(s: String): Unit = {
        split(s, sep, escape) match {
          case Left(x) =>
            acc.add(StringToken(x))
            ()
          case Right((before, separator, after)) =>
            acc.add(StringToken(before))
            acc.add(StringToken(separator.toString()))
            doSplit(after)
        }
      }

      doSplit(s)
    }

    private def split(
        s: String,
        sep: Set[Char],
        escape: Option[(Char, Char)]
    ): Either[String, (String, Char, String)] = {
      escape match {
        case None    => splitNoEscape(s, sep)
        case Some(e) => splitWithEscape(s, sep, e)
      }
    }

    private def splitNoEscape(s: String, sep: Set[Char]): Either[String, (String, Char, String)] = {
      val i = s.indexWhere(sep.contains)
      if (i == -1) Left(s)
      else Right((s.substring(0, i), s.charAt(i), s.substring(i + 1)))
    }

    private def splitWithEscape(
        s: String,
        sep: Set[Char],
        escape: (Char, Char)
    ): Either[String, (String, Char, String)] = {
      val sLength = s.length
      @tailrec
      def run(i: Int, inEscape: Boolean): Either[String, (String, Char, String)] = {
        if (i == sLength) Left(s)
        else {
          val c = s(i)
          if (inEscape && c == escape._2) {
            run(i + 1, inEscape = false)
          } else if (!inEscape && c == escape._1) {
            run(i + 1, inEscape = true)
          } else if (!inEscape && sep.contains(c)) {
            Right((s.substring(0, i), s.charAt(i), s.substring(i + 1)))
          } else run(i + 1, inEscape)
        }
      }

      run(0, inEscape = false)
    }
  }

  sealed trait UriBuilder {
    def fromTokens(u: Uri, t: ArrayView[Token]): (Uri, ArrayView[Token])
  }

  object UriBuilder {
    case object Scheme extends UriBuilder {
      override def fromTokens(u: Uri, t: ArrayView[Token]): (Uri, ArrayView[Token]) = {
        split(t, SchemeEnd) match {
          case Left(_) => (u, t)
          case Right((schemeTokens, _, otherTokens)) =>
            val scheme = tokensToString(schemeTokens)
            (u.scheme(scheme), otherTokens)
        }
      }
    }

    case object UserInfo extends UriBuilder {
      override def fromTokens(u: Uri, t: ArrayView[Token]): (Uri, ArrayView[Token]) = {
        split(t, AtInAuthority) match {
          case Left(tt) => (u, tt)
          case Right((uiTokens, _, otherTokens)) =>
            (uiFromTokens(u, uiTokens), otherTokens)
        }
      }

      private def uiFromTokens(u: Uri, uiTokens: ArrayView[Token]): Uri = {
        val uiTokensWithDots = uiTokens.map {
          case DotInAuthority => StringToken(".")
          case x              => x
        }
        split(uiTokensWithDots, ColonInAuthority) match {
          case Left(tt) => uiFromTokens(u, tt, ArrayView.empty)
          case Right((usernameTokens, _, passwordTokens)) =>
            uiFromTokens(u, usernameTokens, passwordTokens)
        }
      }

      private def uiFromTokens(u: Uri, usernameTokens: ArrayView[Token], passwordTokens: ArrayView[Token]): Uri = {
        (tokensToStringOpt(usernameTokens), tokensToStringOpt(passwordTokens)) match {
          case (Some(un), Some(p)) => u.userInfo(un, p)
          case (Some(un), None)    => u.userInfo(un)
          case (None, Some(p))     => u.userInfo("", p)
          case (None, None)        => u
        }
      }
    }

    case object HostPort extends UriBuilder {
      override def fromTokens(u: Uri, t: ArrayView[Token]): (Uri, ArrayView[Token]) = {
        split(t, AuthorityEnd) match {
          case Left(tt) if tt.lastOption.contains(AuthorityEnd) => (hostPortFromTokens(u, tt), ArrayView.empty)
          case Left(tt)                                         => (u, tt)
          case Right((hpTokens, _, otherTokens))                => (hostPortFromTokens(u, hpTokens), otherTokens)
        }
      }

      private def hostPortFromTokens(u: Uri, rawHpTokens: ArrayView[Token]): Uri = {
        if (rawHpTokens.isEmpty) {
          u // no authority
        } else {
          hostPortFromNonemptyTokens(u, rawHpTokens)
        }
      }

      private def hostPortFromNonemptyTokens(u: Uri, rawHpTokens: ArrayView[Token]): Uri = {
        // Special case: if the host/port part contains an expression token,
        // which has a string representation which contains a colon (:), then
        // we assume that the intention was to embed the port and host separately,
        // not to escape the colon in the host name.
        val hpTokens = rawHpTokens.flatMapLike {
          case e: ExpressionToken =>
            val es = anyToString(e.e)
            es.split(":", 2) match {
              case Array(h, p) if p.matches("\\d+") =>
                StringToken(h) :: ColonInAuthority :: StringToken(p) :: Nil
              case _ => e :: Nil
            }
          case t => t :: Nil
        }

        if (hpTokens.count(_ == ColonInAuthority) > 1) {
          throw new IllegalArgumentException("port specified multiple times")
        }

        split(hpTokens, ColonInAuthority) match {
          case Left(tt) => hostFromTokens(u, tt)
          case Right((hostTokens, _, portTokens)) =>
            portFromTokens(hostFromTokens(u, hostTokens), portTokens)
        }
      }

      private def hostFromTokens(u: Uri, tokens: ArrayView[Token]): Uri = {
        val hostFragments = tokensToStringSeq(tokens)
        u.host(hostFragments.mkString("."))
      }

      private def portFromTokens(u: Uri, tokens: ArrayView[Token]): Uri = {
        u.port(tokensToStringOpt(tokens).flatMap(ParseUtils.toIntOption))
      }
    }

    case object Path extends UriBuilder {
      override def fromTokens(u: Uri, t: ArrayView[Token]): (Uri, ArrayView[Token]) = {
        val noSchemeAndAuthority = u.scheme.isEmpty && u.authority.isEmpty

        val (uu, tt) = withoutAbsolutePathPrefixTokens(t) match {
          case Some(tt) if noSchemeAndAuthority =>
            // the uri is relative and starts with a /, which will be parsed as an empty initial component - removing
            (u, PathStart +: tt)
          case _ if noSchemeAndAuthority && t.headOption.contains(PathStart) =>
            // the uri is relative and the path is relative as well - doesn't start with /
            (u.copy(pathSegments = Uri.RelativePath(Nil)), t)
          case _ => (u, t)
        }

        fromStartingToken(uu, tt, PathStart, Set[Token](QueryStart, FragmentStart), pathFromTokens)
      }

      private def pathFromTokens(u: Uri, tokens: ArrayView[Token]): Uri = {
        u.addPath(tokensToStringSeq(tokens))
      }

      private def withoutAbsolutePathPrefixTokens(t: ArrayView[Token]): Option[ArrayView[Token]] =
        if (t.startsWith(PathStart)) {
          // there might be multiple empty string tokens, in case of an initial expression token with an absolute path
          val t2 = t.drop(1).dropWhile(_ == StringToken(""))
          if (t2.startsWith(SlashInPath)) Some(t2.drop(1)) else None
        } else None
    }

    case object Query extends UriBuilder {
      import Uri.{QuerySegment => QF}

      override def fromTokens(u: Uri, t: ArrayView[Token]): (Uri, ArrayView[Token]) =
        fromStartingToken(u, t, QueryStart, Set[Token](FragmentStart), queryFromTokens)

      private def queryFromTokens(u: Uri, tokens: ArrayView[Token]): Uri = {
        val qfs =
          splitToGroups(tokens, AmpInQuery)
            .flatMap(queryMappingsFromTokens)

        u.copy(querySegments = qfs)
      }

      private def queryMappingsFromTokens(tokens: ArrayView[Token]): Vector[QF] = {
        def expressionPairToQueryFragment(ke: Any, ve: Any): Option[QF.KeyValue] =
          for {
            k <- anyToStringOpt(ke)
            v <- anyToStringOpt(ve)
          } yield QF.KeyValue(k, v)

        def seqToQueryFragments(s: Seq[_]): Vector[QF] = {
          s.flatMap {
            case (ke, ve) => expressionPairToQueryFragment(ke, ve)
            case ve       => anyToStringOpt(ve).map(QF.Value(_))
          }.toVector
        }

        split(tokens, EqInQuery) match {
          case Left(Singleton(ExpressionToken(e: Map[_, _]))) =>
            seqToQueryFragments(e.toSeq)
          case Left(Singleton(ExpressionToken(e: Seq[_]))) =>
            seqToQueryFragments(e)
          case Left(Singleton(ExpressionToken(mqp: QueryParams))) =>
            QF.fromQueryParams(mqp).toVector
          case Left(t) => tokensToStringOpt(t, decodePlusAsSpace = true).map(QF.Value(_)).toVector
          case Right((leftEq, _, rightEq)) =>
            tokensToStringOpt(leftEq, decodePlusAsSpace = true) match {
              case Some(k) =>
                tokensToStringSeq(rightEq, decodePlusAsSpace = true).map(QF.KeyValue(k, _)).toVector

              case None =>
                Vector.empty
            }
        }
      }
    }

    case object Fragment extends UriBuilder {
      override def fromTokens(u: Uri, t: ArrayView[Token]): (Uri, ArrayView[Token]) =
        if (t.startsWith(FragmentStart)) {
          (u.fragment(tokensToStringOpt(t.drop(1))), ArrayView.empty)
        } else {
          (u, t)
        }
    }

    /** Parse a prefix of tokens `t` into a component of a URI. The component is only present in the tokens if there's a
      * `startingToken`; otherwise the component is skipped.
      *
      * The component is terminated by any of `nextComponentTokens`.
      */
    private def fromStartingToken(
        u: Uri,
        t: ArrayView[Token],
        startingToken: Token,
        nextComponentTokens: Set[Token],
        componentFromTokens: (Uri, ArrayView[Token]) => Uri
    ): (Uri, ArrayView[Token]) =
      if (t.startsWith(startingToken)) {
        val tt = t.drop(1)
        if (nextComponentTokens.size == 1) {
          split(tt, nextComponentTokens.iterator.next()) match {
            case Left(ttt) =>
              (componentFromTokens(u, ttt), ArrayView.empty)
            case Right((componentTokens, _, otherTokens)) =>
              (componentFromTokens(u, componentTokens), otherTokens.shiftLeft)
          }
        } else {
          tt.indexWhere(nextComponentTokens) match {
            case -1 =>
              (componentFromTokens(u, tt), ArrayView.empty)
            case i =>
              (componentFromTokens(u, tt.take(i)), tt.drop(i))
          }
        }
      } else {
        (u, t)
      }

    private def anyToString(a: Any): String = anyToStringOpt(a).getOrElse("")

    private def anyToStringOpt(a: Any): Option[String] =
      a match {
        case None    => None
        case null    => None
        case Some(x) => Some(x.toString)
        case x       => Some(x.toString)
      }

    /*
    #102: the + sign should be decoded into a space only when it's part of the query. Otherwise, it should be
    kept as-is.
     */
    private def tokensToStringSeq(tokens: ArrayView[Token], decodePlusAsSpace: Boolean = false): Seq[String] = {
      /*
      #40: when converting tokens to a string sequence, we have to look at
      groups of string/expression (value) tokens separated by others. If there
      are multiple tokens in each such group, their string representations
      should be concatenated (corresponds to e.g. $x$y). A single
      collection-valued token should be expanded.
       */

      def isValueToken(t: Token) =
        t match {
          case ExpressionToken(_) => true
          case StringToken(_)     => true
          case EqInQuery          => true // #64: query values can contain = signs as well
          case _                  => false
        }

      val b = new VectorBuilder[String]()

      @tailrec
      def doToSeq(ts: ArrayView[Token]): Unit = {
        val tsWithValuesPrefix = ts.dropWhile(to => !isValueToken(to))
        val (valueTs, tailTs) = tsWithValuesPrefix.span(isValueToken)

        if (valueTs.nonEmpty) {
          valueTs match {
            case Singleton(ExpressionToken(s: Iterable[_])) =>
              b ++= s.flatMap(anyToStringOpt)
              doToSeq(tailTs)
            case Singleton(ExpressionToken(s: Array[_])) =>
              b ++= s.flatMap(anyToStringOpt)
              doToSeq(tailTs)
            case valueTs =>
              val mbStr = valueTs mkStringOpt {
                case StringToken(s) => Some(decode(s, decodePlusAsSpace))
                case ExpressionToken(e) => anyToStringOpt(e)
                case EqInQuery => Some("=")
                case _ => None
              }
              mbStr.foreach(b += _)
              doToSeq(tailTs)
          }
        }
      }

      doToSeq(tokens)
      b.result()
    }

    private def tokensToStringOpt(t: ArrayView[Token], decodePlusAsSpace: Boolean = false): Option[String] =
      if (t.isEmpty) {
        None
      } else {
        t match {
          case Singleton(ExpressionToken(e)) => anyToStringOpt(e)
          case t => Some(tokensToString(t, decodePlusAsSpace))
        }
      }

    private def tokensToString(t: ArrayView[Token], decodePlusAsSpace: Boolean = false): String =
      t.mkString {
        case StringToken(s)     => decode(s, decodePlusAsSpace)
        case ExpressionToken(e) => anyToString(e)
        case _ => ""
      }

    private def split[T](v: ArrayView[T], sep: T): Either[ArrayView[T], (ArrayView[T], T, ArrayView[T])] = {
      val i = v.indexOf(sep)
      if (i == -1) Left(v) else Right((v.take(i), v.get(i), v.drop(i + 1)))
    }

    private def splitToGroups[T](v: ArrayView[T], sep: T): Vector[ArrayView[T]] = {
      val resultSize = v.count(_ == sep)
      val b = new VectorBuilder[ArrayView[T]]()
      b.sizeHint(resultSize + 1)

      @tailrec
      def doSplit(vv: ArrayView[T]): Unit = {
        vv.indexOf(sep) match {
          case -1 => b += vv
          case i => {
            b += vv.take(i)
            doSplit(vv.drop(i + 1))
          }
        }
      }

      doSplit(v)
      b.result()
    }

    private def decode(s: String, decodePlusAsSpace: Boolean): String = Rfc3986.decode(decodePlusAsSpace)(s)
  }

  /** After tokenizing, there might be extra empty string tokens (`StringToken("")`) before and after expressions. For
    * example, `key=$value` will tokenize to:
    *
    * `Vector(StringToken("key"), EqInQuery, StringToken(""), ExpressionToken(value))`
    *
    * These empty string tokens need to be removed so that e.g. extra key-value mappings are not generated.
    */
  private def removeEmptyTokensAroundExp(tokens: JList[Token]): Unit = {
    var prevWasEmptyString = false
    var prevWasExpression = false
    (0 until tokens.size()).foreach { idx =>
      tokens.get(idx) match {
        case StringToken("") =>
          if (prevWasExpression) {
            tokens.set(idx, null)
            prevWasExpression = false
          } else {
            prevWasEmptyString = true
          }
        case ExpressionToken(_) =>
          if (prevWasEmptyString) {
            tokens.set(idx - 1, null)
            prevWasEmptyString = false
          }
          prevWasExpression = true
        case null =>
          ()
        case _ =>
          prevWasExpression = false
          prevWasEmptyString = false
      }
    }
  }

  /** In relative URIs or URIs without authority, there might be no explicit path start (`/`). Adding it so that the
    * second pass of parsing can depend on the `PathStart` token being available.
    */
  private def addPathStartAfterAuthorityOrSchemeEnd(tokens: JList[Token]): Unit = {
    val endIndex = tokens.indexOf(AuthorityEnd) match {
      case -1 => tokens.indexOf(SchemeEnd)
      case n  => n
    }

    nextNonNull(tokens, endIndex + 1).foreach { case (_, afterEndIndex) =>
      if (afterEndIndex != PathStart && afterEndIndex != QueryStart && afterEndIndex != FragmentStart) {
        // no start token after authority/scheme end - inserting
        if (endIndex == -1) {
          tokens.add(0, PathStart)
        } else {
          tokens.add(endIndex + 1, PathStart)
        }
      }
    }
  }

  private def filterNulls(tokens: JList[Token]): ArrayView[Token] = {
    val array = new Array[Token](tokens.size())
    var i = 0
    (0 until tokens.size()).foreach { idx =>
      val token = tokens.get(idx)
      if (token != null) {
        array.update(i, token)
        i += 1
      }
    }

    new ArrayView(array, 0, i)
  }

  def lastNonNull[T](tokens: JList[T], from: Int): Option[(Int, T)] = {
    var i = from
    while (0 <= i && tokens.get(i) == null) {
      i -= 1
    }
    if (i < from) None else Some((i, tokens.get(i)))
  }

  def nextNonNull[T](tokens: JList[T], from: Int): Option[(Int, T)] = {
    var i = from
    val len = tokens.size()
    while (i < len && tokens.get(i) == null) {
      i += 1
    }
    if (i == len) None else Some((i, tokens.get(i)))
  }
}
