import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object HashWithChains {
  def main(args: Array[String]): Unit = {

    val source = Source.fromFile("D:\\CourseraHW\\week3_hash_tables\\2_hash_chains\\tests\\06").getLines().toArray

    val cardinality = source(0).toInt
    val listOfAction = source.drop(2).map(_.split(" ")).map(x => (x(0), x(1)))
    val prime = 1000000007
    val hashFunction = 263

    val hashTable = new HashTable(cardinality, hashFunction, prime)
    var output = new ArrayBuffer[String]
    for (i <- 0 until listOfAction.length) {
//      println("action called: " + listOfAction(i)._1 + " " + listOfAction(i)._2)
      if (listOfAction(i)._1 == "add") {
        hashTable.add(listOfAction(i)._2)
      }
      if (listOfAction(i)._1 == "check") {
        output.append(hashTable.check(listOfAction(i)._2.toInt ))
//        println(hashTable.check(listOfAction(i)._2.toInt))

      }
      if (listOfAction(i)._1 == "find") {
        output.append(hashTable.find(listOfAction(i)._2))
//        println(hashTable.find(listOfAction(i)._2))
      }
      if (listOfAction(i)._1 == "del") {
        hashTable.del(listOfAction(i)._2)
      }
//      hashTable.hashTable.foreach(x => println("hastable print : " + x.mkString(" ")))
    }

//        output.foreach(x => println(x))

  }


}
