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
      
      def getPrevValue(c:Int, r:Int):Int = {
        if (c == 0 || c==r) 1
        else getPrevValue(c-1, r-1) + getPrevValue(c, r-1)  
      
      }
      getPrevValue(c, r)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
 
      def popOne(numOfLeftParen:Int, rest: List[Char]): Boolean = {

        if (rest.isEmpty) numOfLeftParen==0
        else if(numOfLeftParen < 0) false
        else if (rest.head==')') popOne(numOfLeftParen - 1, rest.tail)
        else if (rest.head=='(') popOne(numOfLeftParen + 1, rest.tail)
        else popOne(numOfLeftParen, rest.tail)
        }
      popOne(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int ={
        
      def check(m:Int, cs:List[Int]): Int =
        if(m % cs.head == 0) 1
        else 0
      
      def doLoop(m:Int, cs:List[Int]): Int = {
        if(m < 0 || cs.isEmpty) 0
        else if(cs.tail.isEmpty) check(m, cs)
        else if(m >= cs.head) doLoop(m-cs.head, cs) + doLoop(m, cs.tail)
        else doLoop(m, cs.tail)
      }
      
      doLoop(money, coins)
    }
    
    
  }


