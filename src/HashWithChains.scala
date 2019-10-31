import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable._
import scala.util.control.Breaks

object HashWithChains {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray
    //    val input = Array("5", "12", "add world", "add HellO", "check 4", "find World", "find world", "del world", "check 4", "del HellO", "add luck", "add GooD", "check 2", "del good")
    val cardinality = input(0).toInt
    val listOfAction = input.drop(2).map(_.split(" ")).map(x => (x(0), x(1)))
    val prime = 1000000007
    val hashFunction = 263
    val hashTable = new HashTableThis(cardinality, hashFunction, prime)
    val output = new ArrayBuffer[String]
    var k = 0
    for (i <- 0 until listOfAction.length) {

      //              println("action called at position " + i + " : " + listOfAction(i)._1 + " " + listOfAction(i)._2)
      if (listOfAction(i)._1 == "add") {
        hashTable.add(listOfAction(i)._2)
        //                println("hash of added value: " + hashTable.calculateHashTest(listOfAction(i)._2))
      }
      if (listOfAction(i)._1 == "check") {
        output.append(hashTable.check(listOfAction(i)._2.toInt))
        //                println("calculated output: " + hashTable.check(listOfAction(i)._2.toInt))
        //                println("correct output: " + correctOutput(k))
        k += 1


      }
      if (listOfAction(i)._1 == "find") {
        output.append(hashTable.find(listOfAction(i)._2))
        //                println("calculated output: " + hashTable.find(listOfAction(i)._2))
        //                println("correct output: " + correctOutput(k))

        k += 1
      }
      if (listOfAction(i)._1 == "del") {
        hashTable.del(listOfAction(i)._2)
      }

      /*
                    for(k <- 0 until hashTable.hashTable.length){
                      println("hashtable check "+ k + " : " + hashTable.hashTable(k)   )
                    }
      */

    }

    output.foreach(x => println(x))
  }


}


class HashTableThis(var cardinality: Int, var hashFunction: Int, var prime: Long) {
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
    val dv2 = modulos(dv1, cardinality)
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
    //    println("hashValue is " + hashValue)
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
    hashTable(in).toList.reverse.mkString(" ")
  }


}
