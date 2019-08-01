import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.lang.Math.floorMod

object HashWithChains {
  def main(args: Array[String]): Unit = {

    val source = Source.fromFile("C:\\Users\\Jade Phung\\Documents\\homework2\\week3_hash_tables\\2_hash_chains\\tests\\06").getLines().toArray

    val cardinality = source(0).toInt
    val listOfAction = source.drop(2).map(_.split(" ")).map(x => (x(0), x(1)))
    val prime = 1000000007
    val hashFunction = 263
    val correctOutput = Source.fromFile("C:\\Users\\Jade Phung\\Documents\\homework2\\week3_hash_tables\\2_hash_chains\\tests\\06.a").getLines().toArray
    val hashTable = new HashTable(cardinality, hashFunction, prime)
    val output = new ArrayBuffer[String]
    var k = 0
    //085 106 078 107 075 116 072 120 066 119 082 106
    calculateHashTest("UjNkKtHxBwRj")


    /*
        for (i <- 0 until listOfAction.length) {

          println("action called: " + listOfAction(i)._1 + " " + listOfAction(i)._2)
          if (listOfAction(i)._1 == "add") {
            hashTable.add(listOfAction(i)._2)
            println("hash of added value: " + hashTable.calculateHash(listOfAction(i)._2))
          }
          if (listOfAction(i)._1 == "check") {
            output.append(hashTable.check(listOfAction(i)._2.toInt))
            println("calculated output: " + hashTable.check(listOfAction(i)._2.toInt))
            println("correct output: " + correctOutput(k))
            k += 1


          }
          if (listOfAction(i)._1 == "find") {
            output.append(hashTable.find(listOfAction(i)._2))
            println("calculated output: " + hashTable.find(listOfAction(i)._2))
            println("correct output: " + correctOutput(k))

            k += 1
          }
          if (listOfAction(i)._1 == "del") {
            hashTable.del(listOfAction(i)._2)
          }

          //      hashTable.hashTable.foreach(x => println("hastable print : " + x.mkString(" ")))
        }
    */

    //        output.foreach(x => println(x))

  }

  def calculateHashTest(in: String): Int = {

    val charMap = in.toArray.map(_.toInt)
    var add = 0D

    for (m <- 0 until charMap.length) {
      val nezt = charMap(m)
      println("nezt: " + nezt)
      println("power: " + m)
      add = (add + ((nezt * math.pow(263, m)) % 1000000007 ) ) % 1000000007
      println("added value after mod: " + add)
    }

    val dv1 = add % 1000000007
    println("dv1 = " + dv1)
    val dv2 = dv1 % 43
    println(s"Result = $dv2")
    //println("after to Int: " + (add % 1000000007 % 43).toInt)
    //(add % 1000000007 % 43).toInt
    dv2.toInt
  }

}
