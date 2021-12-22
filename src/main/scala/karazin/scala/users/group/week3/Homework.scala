package karazin.scala.users.group.week3

import scala.annotation.tailrec

object Homework:

  // Peano numbers
  abstract class Nat:
    def isZero: Boolean

    def predecessor: Nat

    def successor: Nat = new Succ(this)

    infix def +(that: Nat): Nat

    infix def -(that: Nat): Nat

    // Optional task
    def toInt: Int

    // Optional task
    def fromInt(num: Int) = {
      require(num >= 0, "Not equal to zero")

      @tailrec
      def fromIntRec(num: Int, accept: Nat): Nat =
        if num == 0
        then accept
        else fromIntRec(num - 1, accept.successor)

      fromIntRec(num, Zero)
    }

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type

  object Zero extends Nat :
    def isZero: Boolean = true

    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")

    infix def +(that: Nat): Nat = that

    infix def -(that: Nat): Nat = {
      require(that.isZero, "0 does not follow any natural number ")
      this
    }

    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"

    override def equals(obj: Any): Boolean = obj match {
      case zero: Zero => true
      case _ => false
    }

  class Succ(num: Nat) extends Nat :
    def this(num: Int) = this(Zero.fromInt(num))

    def isZero: Boolean = false

    def predecessor: Nat = num

    infix def +(that: Nat): Nat = new Succ(num + that)

    infix def -(that: Nat): Nat = if that.isZero then this else num - that.predecessor

    // Optional task
    def toInt: Int = {
      @tailrec
      def toIntRec(num: Nat, accept: Int): Int =
        if num.isZero
        then accept
        else toIntRec(num.predecessor, accept + 1)

      toIntRec(num, accept = 1)
    }

    override def equals(obj: Any): Boolean = obj match {
      case zero: Zero => false
      case nat: Nat => this.predecessor == nat.predecessor
      case _ => false
    }

