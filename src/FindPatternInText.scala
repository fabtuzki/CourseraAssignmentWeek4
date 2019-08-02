import scala.io.Source

object FindPatternInText {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("D:\\CourseraHW\\week3_hash_tables\\3_hash_substring\\tests\\06").getLines().toArray
    val pattern = source(0)
    val text = source(1)

    val rabinKarp = new RabinKarpFindPattern

    val output = rabinKarp.findPattern(text, pattern, 1000000007)
    output.foreach( x => println(x))
  }
}
