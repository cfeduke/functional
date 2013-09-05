import scala.annotation.tailrec

object Fibonacci {
  def calculate(nth: Int): Int = {
    @tailrec
    def calc(first: Int, second: Int, count: Int): Int = {
      count match {
        case 0 => first + second
        case _ => calc(second, first + second, count - 1)
      }
    }

    require(nth - 3 > 0)
    calc(0, 1, nth - 3)
  }
}
