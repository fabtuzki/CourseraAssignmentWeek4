import scala.io.Source

object FindPatternInText {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("C:\\Users\\Jade Phung\\Documents\\homework2\\week3_hash_tables\\3_hash_substring\\tests\\06").getLines().toArray
    val pattern = source(0)
    val text = source(1)
    val rabinKarp = new RabinKarpFindPattern(1000000007)

   val out =  rabinKarp.findPattern(text, pattern)


    out.foreach(x => println(x))
  }
}
