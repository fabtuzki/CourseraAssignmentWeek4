import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

class RabinKarpFindPattern {
  def areEqual(subString: String, pattern: String): Boolean = {
    var check = true
    if (subString.length != pattern.length) {
      false
    } else {
      val loop = new Breaks
      loop.breakable {
        for (i <- 0 until pattern.length) {
          if (pattern(i) != subString(i)) {
            var check = false
            loop.break()
          }

        }

      }
      check
    }
  }

  def polyHash(pattern: String, prime: Int, hashFunction: Int): Long = {
    var hash = 0L
    val patternArr = pattern.toArray.map(x => charHash(x))
    for (i <- (0 until pattern.length).reverse) {
      hash = modulos((hash * hashFunction + patternArr(i)).toDouble, prime)
    }
    hash
  }

  def charHash(char: Char): Int = {
    char.toInt
  }

  def precomputeHash(text: String, patternLength: Int, prime: Int, hashFunction: Int): Array[Long] = {
    val prehashedArr = Array.ofDim[Long](text.length - patternLength + 1)
    val lastPattern = text.substring(text.length - patternLength, text.length)
    //Update last element of prehashed array
    prehashedArr.update(text.length - patternLength, polyHash(lastPattern, prime, hashFunction))
    var const = 1L
    for (i <- 1 until patternLength) {
      const = modulos(const * hashFunction, prime)
    }
    for (k <- (0 until text.length - patternLength - 1).reverse) {
      val toUpdate = hashFunction * prehashedArr(k + 1) + charHash(text(k)) - const * charHash(text(k + patternLength))
      prehashedArr.update(k, modulos(toUpdate, prime))
    }
    prehashedArr
  }

  def findPattern(text: String, pattern: String, prime: Int): Array[Int] = {
    val hashFunction = scala.util.Random.nextInt(prime - 1)
    val result = new ArrayBuffer[Int]
    val hashedPattern = polyHash(pattern, prime, hashFunction)
    val precomputedHashArr = precomputeHash(text, pattern.length, prime, hashFunction)
    for (i <- 0 until text.length - pattern.length) {
      if (hashedPattern == precomputedHashArr(i)) {
        if (areEqual(text.substring(i, pattern.length), pattern)) {
          result.append(i)
        }
      }
    }
    result.toArray
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
