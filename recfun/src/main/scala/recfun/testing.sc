import recfun.Main._

def countChange(money: Int, coins: List[Int]): Int = {
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


def countChange2(money: Int, coins: List[Int]): Int = {
  def loop(moneyLeftToBeExchanged: Int, aCoins: List[Int]): Int = {
    if(moneyLeftToBeExchanged == 0) 1
    else if (moneyLeftToBeExchanged < 0 || aCoins.isEmpty) 0
    else {
      loop(moneyLeftToBeExchanged, aCoins.tail) +
        loop(moneyLeftToBeExchanged - aCoins.head, aCoins)
    }
  }
  loop(money, coins)
}



countChange2(12,List(2,4,6))