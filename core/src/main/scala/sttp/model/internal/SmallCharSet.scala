package sttp.model.internal

case class FastCharSet(set: Set[Char]) {

  private val (minCode, maxCode, array) = {
    val codes = set.map(_.toInt)
    val minCode = codes.min
    val maxCode = codes.max
    val array = new Array[Boolean](maxCode - minCode + 1)
    codes.foreach { code =>
      array.update(code - minCode, true)
    }
    (minCode, maxCode, array)
  }

  def contains(ch: Char): Boolean = {
    val code = ch.toInt
    if (minCode <= code && code <= maxCode) {
      array(code - minCode)
    } else {
      false
    }
  }
}

case class FastCharMap[V](map: Map[Char, V]) {

  private val (minCode, maxCode, array) = {
    val codes = map.map { case (key, value) => (key.toInt, value) }
    val minCode = codes.minBy(_._1)._1
    val maxCode = codes.maxBy(_._1)._1
    val array = Array.fill[Option[V]](maxCode - minCode + 1)(None)
    codes.foreach { case (code, value) =>
      array.update(code - minCode, Some(value))
    }
    (minCode, maxCode, array)
  }

  def get(ch: Char): Option[V] = {
    val code = ch.toInt
    if (minCode <= code && code <= maxCode) {
      array(code - minCode)
    } else {
      None
    }
  }

  val keySet: FastCharSet =
    FastCharSet(map.keySet)
}
