import scala.collection.mutable._
import scala.util.control.Breaks

class HashTable(var cardinality: Int, var hashFunction: Int, var prime: Long) {
  val hashTable = Array.fill[ListBuffer[String]](cardinality)(ListBuffer())

  def calculateHash(in: String): Int = {

    val charMap = in.toIterator.map(_.toInt)
    var add = 0D
    var power = 0
    while (charMap.hasNext) {
      val nezt = charMap.next()
      add += nezt * math.pow(hashFunction, power)
      power += 1
    }
    (add % prime % cardinality).toInt
  }

  def add(in: String): Unit = {
    val hashValue = calculateHash(in)
    if (find(in) == "no") {
      hashTable.update(hashValue, hashTable(hashValue) += in)
    }
  }

  def del(in: String): Unit = {
    val hashValue = calculateHash(in)
    if (find(in) == "yes") {
      hashTable.update(hashValue, hashTable(hashValue) -= in)

    }


  }

  def find(in: String): String = {
    var check = "no"
    val hashValue = calculateHash(in)
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
    if (hashTable.isEmpty || hashTable(in).isEmpty) {
      " "
    } else {
      hashTable(in).toList.mkString(" ")
    }
  }


}
