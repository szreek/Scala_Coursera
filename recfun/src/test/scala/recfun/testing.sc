import recfun.Main._

def countChange(money: Int, coins: List[Int]): Int = {
  def helper(toComp: List[Int], cLeft: List[Int], counter: Int): Int =
    (toComp, cLeft) match {
      case (Nil, Nil) => counter
      case (x::xs, Nil) => if (money % x == 0) helper(xs, coins, counter+1)
      else helper(xs, coins, counter)
      case (x::xs, y::ys) =>if ((money % x ==0) && (y % x == 0)) helper(x::xs, ys, counter + money/y - 1)
      else helper(x::xs, ys, counter)
      case (Nil, y::ys) => counter
    }
  helper(coins, coins.tail, 0)
}

countChange(4,List(1,2))