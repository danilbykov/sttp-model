package sttp.model

import sttp.model.internal.{ParseUtils, Rfc3986}

import scala.annotation.tailrec

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
    val isScEmptyString = sc.parts.map(_.trim).forall(_.equals(""))
    val areArgsEmptyString = args.forall(_.equals(""))
    if (isScEmptyString && areArgsEmptyString) {
      throw new IllegalArgumentException("empty string is not valid uri")
    }
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
      builders.foldLeft((startingUri, tokens)) { case ((u, t), builder) =>
        builder.fromTokens(u, t)
      }

    if (leftTokens.nonEmpty) {
      throw new IllegalStateException(s"Tokens left after building the whole uri: $leftTokens, result so far: $uri")
    }

    uri
  }

  private def tokenize(sc: StringContext, args: Any*): Vector[Token] = {
    val strings = sc.parts.iterator
    val expressions = args.iterator

    var (tokenizer, tokens) = Tokenizer.Scheme.tokenize(strings.next())

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
      if (tokens == Vector(StringToken("")) && (nextExpressionStr.contains("://") || !nextStr.contains("://"))) {
        def tokenizeExpressionAsString(): Unit = {
          val (nextTokenizer, nextTokens) =
            tokenizer.tokenize(nextExpression.toString)
          tokenizer = nextTokenizer
          tokens = tokens ++ nextTokens
        }

        def tokenizeStringRemoveEmptyPrefix(): Unit = {
          val (nextTokenizer, nextTokens) = tokenizer.tokenize(nextStr)
          tokenizer = nextTokenizer

          // we need to remove empty tokens around exp as well - however here
          // by hand, as the expression token is unwrapped, so removeEmptyTokensAroundExp
          // won't handle this.
          val nextTokensWithoutEmptyPrefix = nextTokens match {
            case StringToken("") +: tail => tail
            case x                       => x
          }

          def isSlash(t: Token) = t == SlashInPath || t == PathStart

          // remove trailing slash when path is added to an interpolated uri:
          // val a = uri"http://example.com/" // notice the trailing slash
          // val b = uri"$a/xy" // "http://example.com/xy"
          (tokens, nextTokensWithoutEmptyPrefix) match {
            case (ts :+ t :+ StringToken(""), SlashInPath +: nt) if isSlash(t) => tokens = ts ++ (t +: nt)
            case _ => tokens = tokens ++ nextTokensWithoutEmptyPrefix
          }
        }

        tokenizeExpressionAsString()
        tokenizeStringRemoveEmptyPrefix()
      } else {
        tokens = tokens :+ ExpressionToken(nextExpression)

        val (nextTokenizer, nextTokens) = tokenizer.tokenize(nextStr)
        tokenizer = nextTokenizer
        tokens = tokens ++ nextTokens
      }
    }

    val tokensWithEndToken = tokens ++ tokenizer.endToken.toVector
    addPathStartAfterAuthorityOrSchemeEnd(removeEmptyTokensAroundExp(tokensWithEndToken))
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
    def tokenize(s: String): (Tokenizer, Vector[Token])
    def endToken: Option[Token] = None // token to add if the input is exhausted
  }

  object Tokenizer {
    private val AuthorityTerminators = Set('/', '?', '#')

    object Scheme extends Tokenizer {
      private val SchemePattern = "[A-Za-z][A-Za-z0-9+.-]*".r

      override def tokenize(s: String): (Tokenizer, Vector[Token]) = {
        SchemePattern.findPrefixOf(s) match {
          // #59: if the entire string matches the pattern, then there's no scheme terminator (`:`). This means there's
          // no scheme, hence - tokenizing as a relative uri.
          case Some(scheme) if scheme.length == s.length => AfterScheme.tokenize(scheme)
          case _ if s.isEmpty => (this, Vector(StringToken(""))) // scheme (or another component) might be continued
          case Some(scheme) if s(scheme.length) == ':' =>
            val rest = s.substring(scheme.length + 1)
            val (next, afterSchemeTokens) = AfterScheme.tokenize(rest)
            (next, Vector(StringToken(scheme), SchemeEnd) ++ afterSchemeTokens)
          case _ if s.startsWith(":") => // there was an expression token before, end of scheme
            val (next, tokens) = AfterScheme.tokenize(s.substring(1))
            (next, SchemeEnd +: tokens)
          case _ => // no scheme
            AfterScheme.tokenize(s)
        }
      }

      override def endToken: Option[Token] = Some(SchemeEnd)
    }

    object AfterScheme extends Tokenizer {
      override def tokenize(s: String): (Tokenizer, Vector[Token]) = {
        if (s == "") (this, Vector(StringToken("")))
        else if (s.startsWith("//")) Authority.tokenize(s.substring(2)) // uri with authority
        else { // uri without authority
          val first = s(0)
          if (AuthorityTerminators.contains(first)) {
            val (tokenizer, token) = separatorTokenizerAndToken(first)
            val tokens1 = if (token == PathStart) {
              // absolute path in a relative uri, adding an empty string token so that the absolute path is preserved
              // (might be a continuation if there was no scheme)
              Vector(StringToken(""), SlashInPath)
            } else Vector(token)
            val (tokenizer2, tokens2) = tokenizer.tokenize(s.substring(1))
            (tokenizer2, tokens1 ++ tokens2)
          } else {
            // non-slash-initiated path (might be a continuation if there was no scheme)
            Path.tokenize(s)
          }
        }
      }
    }

    object Authority extends Tokenizer {
      private val IpV6InAuthorityPattern = "\\[[0-9a-fA-F:]+\\]".r // see the pattern in Uri.HostEncoding

      override def tokenize(s: String): (Tokenizer, Vector[Token]) = {
        val (tokenizer, tokens) = tokenizeTerminatedFragment(
          s,
          this,
          Set('/', '?', '#'),
          Map(':' -> ColonInAuthority, '@' -> AtInAuthority, '.' -> DotInAuthority),
          Some(('[', ']'))
        )
        val tokens2 = tokens.map {
          case StringToken(s @ IpV6InAuthorityPattern()) =>
            // removing the [] which are used to surround ipv6 addresses in URLs
            StringToken(s.substring(1, s.length - 1))
          case t => t
        }
        (tokenizer, tokens2)
      }

      override def endToken: Option[Token] = Some(AuthorityEnd)
    }

    object Path extends Tokenizer {
      override def tokenize(s: String): (Tokenizer, Vector[Token]) =
        tokenizeTerminatedFragment(
          s,
          this,
          Set('?', '#'),
          Map('/' -> SlashInPath)
        )
    }

    object Query extends Tokenizer {
      override def tokenize(s: String): (Tokenizer, Vector[Token]) =
        tokenizeTerminatedFragment(
          s,
          this,
          Set('#'),
          Map('&' -> AmpInQuery, '=' -> EqInQuery)
        )
    }

    object Fragment extends Tokenizer {
      override def tokenize(s: String): (Tokenizer, Vector[Token]) =
        (this, Vector(StringToken(s)))
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
        terminators: Set[Char],
        separatorsToTokens: Map[Char, Token],
        separatorsEscape: Option[(Char, Char)] = None
    ): (Tokenizer, Vector[Token]) = {
      def tokenizeFragment(f: String): Vector[Token] = {
        splitPreserveSeparators(f, separatorsToTokens.keySet, separatorsEscape).map { t =>
          t.headOption.flatMap(separatorsToTokens.get) match {
            case Some(token) => token
            case None        => StringToken(t)
          }
        }
      }

      // first checking if the fragment doesn't end; e.g. the authority is
      // terminated by /, ?, # or end of string (there might be other /, ?,
      // # later on e.g. in the query).
      // See: https://tools.ietf.org/html/rfc3986#section-3.2
      split(s, terminators, None) match {
        case Right((fragment, separator, rest)) =>
          tokenizeAfterSeparator(tokenizeFragment(fragment) ++ current.endToken.toVector, separator, rest)

        case Left(fragment) =>
          (current, tokenizeFragment(fragment))
      }
    }

    private def tokenizeAfterSeparator(
        beforeSeparatorTokens: Vector[Token],
        separator: Char,
        s: String
    ): (Tokenizer, Vector[Token]) = {
      val (next, separatorToken) = separatorTokenizerAndToken(separator)
      val (nextNext, nextTokens) = next.tokenize(s)
      (nextNext, beforeSeparatorTokens ++ Vector(separatorToken) ++ nextTokens)
    }

    private def separatorTokenizerAndToken(separator: Char): (Tokenizer, Token) =
      separator match {
        case '/' => (Path, PathStart)
        case '?' => (Query, QueryStart)
        case '#' => (Fragment, FragmentStart)
      }

    private def splitPreserveSeparators(s: String, sep: Set[Char], escape: Option[(Char, Char)]): Vector[String] = {
      @tailrec
      def doSplit(s: String, acc: Vector[String]): Vector[String] = {
        split(s, sep, escape) match {
          case Left(x) => acc :+ x
          case Right((before, separator, after)) =>
            doSplit(after, acc ++ Vector(before, separator.toString))
        }
      }

      doSplit(s, Vector.empty)
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
    def fromTokens(u: Uri, t: Vector[Token]): (Uri, Vector[Token])
  }

  object UriBuilder {
    case object Scheme extends UriBuilder {
      override def fromTokens(u: Uri, t: Vector[Token]): (Uri, Vector[Token]) = {
        split(t, Set[Token](SchemeEnd)) match {
          case Left(_) => (u, t)
          case Right((schemeTokens, _, otherTokens)) =>
            val scheme = tokensToString(schemeTokens)
            (u.scheme(scheme), otherTokens)
        }
      }
    }

    case object UserInfo extends UriBuilder {
      override def fromTokens(u: Uri, t: Vector[Token]): (Uri, Vector[Token]) = {
        split(t, Set[Token](AtInAuthority)) match {
          case Left(tt) => (u, tt)
          case Right((uiTokens, _, otherTokens)) =>
            (uiFromTokens(u, uiTokens), otherTokens)
        }
      }

      private def uiFromTokens(u: Uri, uiTokens: Vector[Token]): Uri = {
        val uiTokensWithDots = uiTokens.map {
          case DotInAuthority => StringToken(".")
          case x              => x
        }
        split(uiTokensWithDots, Set[Token](ColonInAuthority)) match {
          case Left(tt) => uiFromTokens(u, tt, Vector.empty)
          case Right((usernameTokens, _, passwordTokens)) =>
            uiFromTokens(u, usernameTokens, passwordTokens)
        }
      }

      private def uiFromTokens(u: Uri, usernameTokens: Vector[Token], passwordTokens: Vector[Token]): Uri = {
        (tokensToStringOpt(usernameTokens), tokensToStringOpt(passwordTokens)) match {
          case (Some(un), Some(p)) => u.userInfo(un, p)
          case (Some(un), None)    => u.userInfo(un)
          case (None, Some(p))     => u.userInfo("", p)
          case (None, None)        => u
        }
      }
    }

    case object HostPort extends UriBuilder {
      override def fromTokens(u: Uri, t: Vector[Token]): (Uri, Vector[Token]) = {
        split(t, Set[Token](AuthorityEnd)) match {
          case Left(tt) if tt.lastOption.contains(AuthorityEnd) => (hostPortFromTokens(u, tt), Vector.empty)
          case Left(tt)                                         => (u, tt)
          case Right((hpTokens, _, otherTokens))                => (hostPortFromTokens(u, hpTokens), otherTokens)
        }
      }

      private def hostPortFromTokens(u: Uri, rawHpTokens: Vector[Token]): Uri = {
        if (rawHpTokens.isEmpty) {
          u // no authority
        } else {
          hostPortFromNonemptyTokens(u, rawHpTokens)
        }
      }

      private def hostPortFromNonemptyTokens(u: Uri, rawHpTokens: Vector[Token]): Uri = {
        // Special case: if the host/port part contains an expression token,
        // which has a string representation which contains a colon (:), then
        // we assume that the intention was to embed the port and host separately,
        // not to escape the colon in the host name.
        val hpTokens = rawHpTokens.flatMap {
          case e: ExpressionToken =>
            val es = tokensToString(Vector(e))
            es.split(":", 2) match {
              case Array(h, p) if p.matches("\\d+") =>
                Vector(StringToken(h), ColonInAuthority, StringToken(p))
              case _ => Vector(e)
            }
          case t => Vector(t)
        }

        if (hpTokens.head.equals(StringToken("")) && hpTokens.drop(1).headOption.contains(DotInAuthority)) {
          throw new IllegalArgumentException("incorrect hostname")
        }

        if (hpTokens.count(_ == ColonInAuthority) > 1) {
          throw new IllegalArgumentException("port specified multiple times")
        }

        split(hpTokens, Set[Token](ColonInAuthority)) match {
          case Left(tt) => hostFromTokens(u, tt)
          case Right((hostTokens, _, portTokens)) =>
            portFromTokens(hostFromTokens(u, hostTokens), portTokens)
        }
      }

      private def hostFromTokens(u: Uri, tokens: Vector[Token]): Uri = {
        val hostFragments = tokensToStringSeq(tokens)
        u.host(hostFragments.mkString("."))
      }

      private def portFromTokens(u: Uri, tokens: Vector[Token]): Uri = {
        u.port(tokensToStringOpt(tokens).flatMap(ParseUtils.toIntOption))
      }
    }

    case object Path extends UriBuilder {
      override def fromTokens(u: Uri, t: Vector[Token]): (Uri, Vector[Token]) = {
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

      private def pathFromTokens(u: Uri, tokens: Vector[Token]): Uri = {
        u.addPath(tokensToStringSeq(tokens))
      }

      private def withoutAbsolutePathPrefixTokens(t: Vector[Token]): Option[Vector[Token]] =
        if (t.startsWith(List(PathStart))) {
          // there might be multiple empty string tokens, in case of an initial expression token with an absolute path
          val t2 = t.tail.dropWhile(_ == StringToken(""))
          if (t2.headOption.contains(SlashInPath)) Some(t2.tail) else None
        } else None
    }

    case object Query extends UriBuilder {
      import Uri.{QuerySegment => QF}

      override def fromTokens(u: Uri, t: Vector[Token]): (Uri, Vector[Token]) =
        fromStartingToken(u, t, QueryStart, Set[Token](FragmentStart), queryFromTokens)

      private def queryFromTokens(u: Uri, tokens: Vector[Token]): Uri = {
        val qfs =
          splitToGroups(tokens, AmpInQuery)
            .flatMap(queryMappingsFromTokens)

        u.copy(querySegments = qfs)
      }

      private def queryMappingsFromTokens(tokens: Vector[Token]): Vector[QF] = {
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

        split(tokens, Set[Token](EqInQuery)) match {
          case Left(Vector(ExpressionToken(e: Map[_, _]))) =>
            seqToQueryFragments(e.toSeq)
          case Left(Vector(ExpressionToken(e: Seq[_]))) =>
            seqToQueryFragments(e)
          case Left(Vector(ExpressionToken(mqp: QueryParams))) =>
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
      override def fromTokens(u: Uri, t: Vector[Token]): (Uri, Vector[Token]) = {
        t match {
          case FragmentStart +: tt =>
            (u.fragment(tokensToStringOpt(tt)), Vector.empty)

          case _ => (u, t)
        }
      }
    }

    /** Parse a prefix of tokens `t` into a component of a URI. The component is only present in the tokens if there's a
      * `startingToken`; otherwise the component is skipped.
      *
      * The component is terminated by any of `nextComponentTokens`.
      */
    private def fromStartingToken(
        u: Uri,
        t: Vector[Token],
        startingToken: Token,
        nextComponentTokens: Set[Token],
        componentFromTokens: (Uri, Vector[Token]) => Uri
    ): (Uri, Vector[Token]) = {
      t match {
        case `startingToken` +: tt =>
          split(tt, nextComponentTokens) match {
            case Left(ttt) =>
              (componentFromTokens(u, ttt), Vector.empty)
            case Right((componentTokens, sep, otherTokens)) =>
              (componentFromTokens(u, componentTokens), sep +: otherTokens)
          }

        case _ => (u, t)
      }
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
    private def tokensToStringSeq(tokens: Vector[Token], decodePlusAsSpace: Boolean = false): Seq[String] = {
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

      @tailrec
      def doToSeq(ts: Vector[Token], acc: Vector[String]): Seq[String] = {
        val tsWithValuesPrefix = ts.dropWhile(to => !isValueToken(to))
        val (valueTs, tailTs) = tsWithValuesPrefix.span(isValueToken)

        valueTs match {
          case Vector() => acc // tailTs must be empty then as well
          case Vector(ExpressionToken(s: Iterable[_])) =>
            doToSeq(tailTs, acc ++ s.flatMap(anyToStringOpt).toVector)
          case Vector(ExpressionToken(s: Array[_])) =>
            doToSeq(tailTs, acc ++ s.flatMap(anyToStringOpt).toVector)
          case _ =>
            val values = valueTs
              .flatMap {
                case ExpressionToken(e) => anyToStringOpt(e)
                case StringToken(s)     => Some(decode(s, decodePlusAsSpace))
                case EqInQuery          => Some("=")
                case _                  => None
              }

            val strToAdd =
              if (values.isEmpty) None else Some(values.mkString(""))

            doToSeq(tailTs, acc ++ strToAdd)
        }
      }

      doToSeq(tokens, Vector.empty)
    }

    private def tokensToStringOpt(t: Vector[Token], decodePlusAsSpace: Boolean = false): Option[String] =
      t match {
        case Vector()                   => None
        case Vector(ExpressionToken(e)) => anyToStringOpt(e)
        case _                          => Some(tokensToString(t, decodePlusAsSpace))
      }

    private def tokensToString(t: Vector[Token], decodePlusAsSpace: Boolean = false): String =
      t.collect {
        case StringToken(s)     => decode(s, decodePlusAsSpace)
        case ExpressionToken(e) => anyToString(e)
      }.mkString("")

    private def split[T](v: Vector[T], sep: Set[T]): Either[Vector[T], (Vector[T], T, Vector[T])] = {
      val i = v.indexWhere(sep.contains)
      if (i == -1) Left(v) else Right((v.take(i), v(i), v.drop(i + 1)))
    }

    private def splitToGroups[T](v: Vector[T], sep: T): Vector[Vector[T]] = {
      @tailrec
      def doSplit(vv: Vector[T], acc: Vector[Vector[T]]): Vector[Vector[T]] = {
        vv.indexOf(sep) match {
          case -1 => acc :+ vv
          case i  => doSplit(vv.drop(i + 1), acc :+ vv.take(i))
        }
      }

      doSplit(v, Vector.empty)
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
  private def removeEmptyTokensAroundExp(tokens: Vector[Token]): Vector[Token] = {
    @tailrec
    def doRemove(t: Vector[Token], acc: Vector[Token]): Vector[Token] =
      t match {
        case StringToken("") +: (e: ExpressionToken) +: tail => doRemove(e +: tail, acc)
        case (e: ExpressionToken) +: StringToken("") +: tail => doRemove(tail, acc :+ e)
        case v +: tail                                       => doRemove(tail, acc :+ v)
        case _ /* empty */                                   => acc
      }

    doRemove(tokens, Vector.empty)
  }

  /** In relative URIs or URIs without authority, there might be no explicit path start (`/`). Adding it so that the
    * second pass of parsing can depend on the `PathStart` token being available.
    */
  private def addPathStartAfterAuthorityOrSchemeEnd(tokens: Vector[Token]): Vector[Token] = {
    val endIndex = tokens.indexOf(AuthorityEnd) match {
      case -1 => tokens.indexOf(SchemeEnd)
      case n  => n
    }

    if (endIndex + 1 == tokens.length) {
      tokens // no path, query or fragment at all
    } else {
      val afterEndIndex = tokens(endIndex + 1)
      if (afterEndIndex != PathStart && afterEndIndex != QueryStart && afterEndIndex != FragmentStart) {
        // no start token after authority/scheme end - inserting
        val (init, tail) = if (endIndex == -1) (Vector.empty, tokens) else tokens.splitAt(endIndex + 1)
        init ++ Vector(PathStart) ++ tail
      } else {
        tokens
      }
    }
  }
}
