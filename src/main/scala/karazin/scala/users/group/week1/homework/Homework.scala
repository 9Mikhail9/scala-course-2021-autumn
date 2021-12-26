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

object Homework:
  //===========================================================================
  object `Boolean Operators`:

    def not(b: Boolean): Boolean =
      if (b)
        false
      else
        true

    end not
    //===========================================================================

    def and(left: Boolean, right: Boolean): Boolean =
      if (left)
        if right then true else false
      else
        false
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
  object `Fermat Numbers`:

    val multiplication: (BigInt, BigInt) => BigInt = (Left, Right) => {
      @tailrec
      def inner(Left: BigInt, Right: BigInt, n: BigInt): BigInt = {
        if (Left == 0) n
        else inner(Left - 1, Right, n + Right)
      }

      inner(Left, Right, 0)
    }
    //===========================================================================
    val power: (BigInt, BigInt) => BigInt = (left, right) => {
      @tailrec
      def inner(left: BigInt, right: BigInt, ant: BigInt): BigInt = {
        if (right == 0) ant
        else inner(left, right - 1, multiplication(ant, left))
      }

      inner(left, right, 1)
    }
    //===========================================================================
    val fermatNumber: Int => BigInt = n => {power(2, power(2, n)) + 1}


  //===========================================================================
  object `Look-and-say Sequence`:
    def toLookAndSay(num: BigInt): BigInt = {
      // converting n to list for convenience
      val nList = num.toString.map(_.asDigit).toList

      @tailrec
      def inner(ints: List[Int], current: BigInt, cnt: Int, accept: BigInt): BigInt = ints match {
        case List() => (accept * 10 + cnt) * 10 + current
        case h :: t => {
          if (h == current) inner(t, current, cnt + 1, accept)
          else inner(t, h, 1, (accept * 10 + cnt) * 10 + current)
        }
      }

      inner(nList.tail, nList.head, 1, 0)
    }

    val lookAndSaySequenceElement: Int => BigInt = num => {
      @tailrec
      def loop(current: BigInt, left: BigInt): BigInt = left match {
        case 0 => current
        case _ => loop(toLookAndSay(current), left - 1)
      }

      loop(1, num)
    }

  end `Look-and-say Sequence`
  //===========================================================================
end Homework