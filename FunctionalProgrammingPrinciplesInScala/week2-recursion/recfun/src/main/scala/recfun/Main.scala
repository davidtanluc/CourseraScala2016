package recfun

import scala.annotation.tailrec

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
    def pascal(c: Int, r: Int): Int = (c,r) match{
    case (0,_)=>1
    case (c1,r1)if c1==r1=>1
    case (_,_)=>pascal(c-1,r-1)+pascal(c,r-1)

  }

  /**
   * Exercise 2
   */

  def balance(chars:List[Char]):Boolean ={
    if(chars.isEmpty)return true

    @tailrec
    def loop1(ch:List[Char],count_opener:Int):Boolean = {

      if(ch.isEmpty && count_opener ==0) return true //balance

      if(ch.isEmpty && count_opener != 0)return false //not balance

      ch match {
        case c if c.head=='('=>loop1(c.tail,count_opener + 1)
        case c if c.head==')'=>if(count_opener>0)loop1(c.tail,count_opener - 1) else false
        case _ =>loop1(ch.tail,count_opener)
      }

    }

    loop1(chars,0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = (money,coins) match {
    case(0,_) =>1 // if money is zero is 1
    case(_,c) if c.isEmpty =>0 //  if list is empty ends
    case(m,_) if m<0 =>0  /// if money is -ve end
    case(m,c::cs)=>countChange(m-c,coins)+ countChange(m,cs)
  }
}
