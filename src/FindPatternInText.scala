import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

object FindPatternInText {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.stdin.getLines().toArray
    val pattern = source(0)
    val text = source(1)
    val rabinKarp = new RabinKarpFindPatternThis(1000000007)
    val out = rabinKarp.findPattern(text, pattern)

    //    val out = rabinKarp.findPattern("abacaba", "aba")
    println(out.mkString(" "))

  }
}


class RabinKarpFindPatternThis(var prime: Int) {
  val hashFunction = scala.util.Random.nextInt(5000)

  def areEqual(subString: String, pattern: String): Boolean = {
    var check = true
    if (subString.length != pattern.length) {
      false
    } else {
      val loop = new Breaks
      loop.breakable {
        for (i <- 0 until pattern.length) {
          if (pattern(i) != subString(i)) {
            check = false
            loop.break()
          }

        }

      }
      check
    }
  }

  def polyHash(pattern: String): Long = {
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

  def precomputeHash(text: String, patternLength: Int): Array[Long] = {
    val prehashedArr = Array.ofDim[Long](text.length - patternLength + 1)
    val lastPattern = text.substring(text.length - patternLength, text.length)
    //Update last element of prehashed array
    //    println("last pattern is : " + lastPattern + " with hashed value: " + polyHash(lastPattern))
    prehashedArr.update(text.length - patternLength, polyHash(lastPattern))
    var const = 1L
    for (i <- 0 until patternLength) {
      const = modulos(const * hashFunction, prime)
      //      println("constant calculated: " + const)
    }
    //    println("constant is " + const)
    for (k <- (0 until text.length - patternLength).reverse) {
      val toUpdate = hashFunction * prehashedArr(k + 1) + charHash(text(k)) - modulos(const * charHash(text(k + patternLength)), prime)
      prehashedArr.update(k, modulos(toUpdate, prime))
      //      println("hashfunction at point " + k + " is " + hashFunction)
      //      println("hashed text at point " + k + " :" + text.substring(k, k + patternLength) + " with hashed code: " + modulos(toUpdate, prime) + " actual poly hash value is " + polyHash(text.substring(k, k + patternLength)))

    }
    prehashedArr
  }

  def findPattern(text: String, pattern: String): Array[Int] = {
    val result = new ArrayBuffer[Int]
    val hashedPattern = polyHash(pattern)
    //    println("hashed pattern is " + hashedPattern)
    val precomputedHashArr = precomputeHash(text, pattern.length)

    //    precomputedHashArr.foreach(x => println("precomputed hash " + x))

    for (i <- 0 until precomputedHashArr.length) {
      if (hashedPattern == precomputedHashArr(i)) {
        if (areEqual(text.substring(i, i + pattern.length), pattern)) {
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
