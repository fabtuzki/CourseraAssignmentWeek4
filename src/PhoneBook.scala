import scala.collection.mutable._
import scala.io.Source

object PhoneBook {
  val output = new ArrayBuffer[String]
  val phoneBook = new HashMap[Int, String]

  def main(args: Array[String]): Unit = {
    val input = Source.stdin.getLines().toArray.drop(1)
    input.foreach(x => action(x))
    output.foreach(x => println(x))

  }

  def action(input: String): Unit = {
    if (input.split(" ")(0) == "add") {
      add(input.split(" ")(1).toInt, input.split(" ")(2))
    }
    if (input.split(" ")(0) == "find") {
      output.append(find(input.split(" ")(1).toInt))
    }
    if (input.split(" ")(0) == "del") {
      del(input.split(" ")(1).toInt)
    }
  }


  def add(number: Int, name: String): Unit = {
    phoneBook.update(number, name)
  }

  def find(number: Int): String = {
    phoneBook.getOrElse(number, "not found")
  }

  def del(number: Int): Unit = {
    phoneBook.remove(number)
  }

}


