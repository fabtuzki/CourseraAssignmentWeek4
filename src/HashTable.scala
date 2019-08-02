import scala.collection.mutable._
import scala.util.control.Breaks

class HashTable(var cardinality: Int, var hashFunction: Int, var prime: Long) {
  val hashTable = Array.fill[ListBuffer[String]](cardinality)(ListBuffer())

  def calculateHashTest(in: String): Int = {

    val charMap = in.toArray.map(_.toInt)
//    charMap.foreach(x => println("ASCII map : " + x))
    var add = 0L

    for (m <- (0 until charMap.length).reverse) {
      val nezt = charMap(m)
//      println("value of ascii : " + nezt)
//      println("power: " + m)
      add = modulos(add * 263 + nezt, 1000000007)
    }

    val dv1 = modulos(add, 1000000007)
    //    println("dv1 = " + dv1)
    val dv2 = modulos(dv1, 43)
//    println(s"Result = $dv2")
    //println("after to Int: " + (add % 1000000007 % 43).toInt)
    //(add % 1000000007 % 43).toInt
    dv2.toInt
  }


  def modulos(dividend: Double, divisor: Long): Long = {
//    println("dividend: " + dividend)
    if (dividend < divisor) {
//      println("dividend is < than divisor")
      dividend.toLong
    } else {
      var remainder = 0D
      val estimatedQ = math.floor(dividend / divisor).toLong
//      println("estimated quotient: " + estimatedQ)
//      println("quotient * divisor: " + estimatedQ * divisor)
      remainder = dividend - estimatedQ * divisor
//      println("remainder is : "  + remainder)
      while (remainder >= divisor) {
//        println("remainder still larger than divisor")
        remainder = remainder - divisor
//        println("subtracted remainder: " + remainder)
      }
      remainder.toLong
    }
  }

  def calculateHash(in: String): Int = {

    val charMap = in.toArray.map(_.toInt)
    var add = 0D
    for (m <- 0 until charMap.length) {
      val nezt = charMap(m)
      add += nezt * math.pow(hashFunction, m)
      //      println("added value" + add)
    }
    //    println("before to Int: " + ((add % prime) % cardinality))
    math.round((add % prime) % cardinality).toInt
  }

  def add(in: String): Unit = {
    val hashValue = calculateHashTest(in)
//    println("hashvalue in add : " + hashValue)
    if (find(in) == "no") {
//      println("update array at such index")
      hashTable.update(hashValue, hashTable(hashValue) += in)
    }
  }

  def del(in: String): Unit = {
    val hashValue = calculateHashTest(in)
    if (find(in) == "yes") {
      hashTable.update(hashValue, hashTable(hashValue) -= in)

    }


  }

  def find(in: String): String = {
    var check = "no"
    val hashValue = calculateHashTest(in)
    val loop = new Breaks
    loop.breakable(
      for (i <- 0 until hashTable(hashValue).length) {
        if (hashTable(hashValue)(i) == in) {
          check = "yes"
          loop.break()
        }
      }
    )
    check

  }

  def check(in: Int): String = {
    hashTable(in).toList.mkString(" ")
  }


}
