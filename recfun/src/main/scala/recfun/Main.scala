package recfun

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
    def pascal(c: Int, r: Int): Int = {
      if(c==r || c==0) 1
      else pascal(c-1,r-1)+pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      var parenthesisCounter = 0

      def balanceIter(chars: List[Char]): Boolean = {
        if(parenthesisCounter<0 || chars.isEmpty) parenthesisCounter == 0
        else {
          if (chars.head == '(') parenthesisCounter+=1
          else if (chars.head == ')' ) parenthesisCounter-=1
          balanceIter(chars.tail)
        }
      }

      balanceIter(chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChangeIter(amount: Int, changeAvailable: List[Int]):Int = {
        if (changeAvailable.isEmpty || amount<0) 0
        else if (amount==0) 1
        else countChangeIter(amount-changeAvailable.head,changeAvailable)+countChangeIter(amount,changeAvailable.tail)
      }

      if(money==0 || coins.isEmpty) 0
      else countChangeIter(money, coins.sortWith(_ > _))
    }
  }
