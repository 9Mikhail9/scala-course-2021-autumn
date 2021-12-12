package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive i s possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :
  //===========================================================================
  object `Boolean Operators` :

    def not(b: Boolean): Boolean =
      if (b)
        false
      else
        true

    end not
    //===========================================================================

    def and(left: Boolean, right: Boolean): Boolean =
      if not(left) then
        false
      else if not(right) then
        false
      else
        true
    end and
    //===========================================================================
    def or(left: Boolean, right: Boolean): Boolean =
      if (left)
        true
      else if (right)
        true
      else
        false
    end or

  end `Boolean Operators`
  //===========================================================================
  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (Left, Right) => {
      @tailrec
      def inner(Left: BigInt, Right: BigInt, n: BigInt): BigInt = {
        if (Left == 0) n
        else inner(Left - 1, Right, n + Right)
      }
      inner(Left, Right, 0)
    }
    //===========================================================================
    val power: (BigInt, BigInt) => BigInt = (Left, Right) => {
      @tailrec
      def inner(Left: BigInt, Right: BigInt, n: BigInt): BigInt = {
        if (Left == 0) n
        else inner(Left,Right - 1,multiplication(n,Left))
      }
      inner(Left, Right,1)
    }
    //===========================================================================
    val fermatNumber: Int => BigInt = n => power(2,power(2,n)) + 1

  end `Fermat Numbers`
  //===========================================================================
  object `Look-and-say Sequence` :
    def lookandsay(n: BigInt): BigInt = {
      val list = n.toString.map(_.asDigit).toList

      @tailrec
      def loop(ints: List[Int], сurrent: BigInt, repeat: Int, times: BigInt): BigInt = {
        if (ints == List()) (times * 10 + repeat) * 10 + сurrent
        else {
          if (ints.head == сurrent) loop(ints.tail, сurrent, repeat + 1, times)
          else loop(ints.tail, ints.head, 1, (times * 10 + repeat) * 10 + сurrent)
        }
      }
      loop(list.tail, list.head, 1, 0)
    }
    val lookAndSaySequenceElement: Int => BigInt = n => {
      @tailrec
      def loop(Сurrent: BigInt, Left: BigInt): BigInt = {
        if (Left == 0) Сurrent
        else loop(lookandsay(Сurrent), Left - 1)
      }
      loop(1, n)
    }
  end `Look-and-say Sequence`
  //===========================================================================
end Homework