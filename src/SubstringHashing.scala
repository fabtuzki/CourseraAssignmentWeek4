class SubstringHashing(var text: String, var prime: Int) {
  def hashFunctionGen(restriction: Int): Int = {
    val hashFunction = scala.util.Random.nextInt(restriction)
    hashFunction
  }

  def polyHash(pattern: String, hashFunction: Int): Long = {
    var hash = 0L
    val patternArr = pattern.toArray.map(x => charHash(x))
    for (i <- (0 until pattern.length).reverse) {
      hash = modulos((hash * hashFunction + patternArr(i)).toDouble, prime)
    }
    hash
  }



  def precomputeHash(text: String, hashFunctionDomain: Int): Array[Long] = {
    val hashFunction = hashFunctionGen(hashFunctionDomain)
    val preHashedArr = Array.ofDim[Long](text.length)
    var hashedValue = modulos(text.charAt(0).toInt, prime)

    preHashedArr.update(0, hashedValue)
    for (i <- 1 until text.length) {
      hashedValue += modulos(text.charAt(i).toInt * math.pow(hashFunction, i), prime)
      hashedValue = modulos(hashedValue, prime)
      preHashedArr.update(i, hashedValue)
    }
    preHashedArr
  }


  def charHash(char: Char): Int = {
    char.toInt
  }

  def modulos(dividend: Double, divisor: Long): Long = {
    if (dividend < divisor) {
      dividend.toLong
    } else {
      var remainder = 0D
      val estimatedQ = math.floor(dividend / divisor).toLong
      remainder = dividend - estimatedQ * divisor
      while (remainder >= divisor) {
        remainder = remainder - divisor
      }

      remainder.toLong

    }
  }


}
