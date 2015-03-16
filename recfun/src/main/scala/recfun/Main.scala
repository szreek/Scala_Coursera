package recfun

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else (pascal(c - 1, r - 1) + pascal(c, r - 1))

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(xs: List[Char], counter: Int): Boolean =
      (xs, counter) match {
        case (Nil, 0) => true
        case (Nil, _) => false
        case ('(' :: xs, _) => helper(xs, counter + 1)
        case (')' :: xs, 0) => false
        case (')' :: xs, _) => helper(xs, counter - 1)
        case (_ :: xs, counter) => helper(xs, counter)
      }
    helper(chars, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def helper(moneyLeft: Int, mCoins: List[Int]): Int =
      if (moneyLeft == 0) 1
      else if (moneyLeft < 0 || mCoins.isEmpty) 0
      else helper(moneyLeft -mCoins.head, mCoins) + helper(moneyLeft, mCoins.tail)
    helper(money, coins)
  }


  /**
   * First try to do the ex 3 with pattern match,
   * lack of combination for more than 2 coins at the same time.
   */

  def countChange2(money: Int, coins: List[Int]): Int = {
    def helper(toComp: List[Int], cLeft: List[Int], counter: Int): Int =
      (toComp, cLeft) match {
        case (Nil, Nil) => counter
        case (x :: Nil, _) => if (money % x == 0) helper(Nil, Nil, counter + 1)
        else helper(Nil, Nil, counter)
        case (x :: xs, Nil) => if (money % x == 0) helper(xs, xs.tail, counter + 1)
        else helper(xs, coins, counter)
        case (x :: xs, y :: ys) => if ((money % x == 0) && (y % x == 0)) helper(x :: xs, ys, counter + money / y - 1)
        else helper(x::xs, ys, counter)
        case (Nil, y :: ys) => counter
      }
    helper(coins, coins.tail, 0)
  }
}
