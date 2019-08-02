import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.lang.Math.floorMod

object HashWithChains {
  def main(args: Array[String]): Unit = {

    val source = Source.fromFile("D:\\CourseraHW\\week3_hash_tables\\2_hash_chains\\tests\\06").getLines().toArray

    val cardinality = source(0).toInt
    val listOfAction = source.drop(2).map(_.split(" ")).map(x => (x(0), x(1)))
    val prime = 1000000007
    val hashFunction = 263
    val correctOutput = Source.fromFile("D:\\CourseraHW\\week3_hash_tables\\2_hash_chains\\tests\\06.a").getLines().toArray
    val hashTable = new HashTable(cardinality, hashFunction, prime)
    val output = new ArrayBuffer[String]
    var k = 0
    //085 106 078 107 075 116 072 120 066 119 082 106
    //hashTable.calculateHashTest("tWLfcAO")

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
