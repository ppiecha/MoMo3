package app.generic
import app.syntax.Extensions.repeat
import cats.data.NonEmptyList

import scala.util.Random

object Rand {

  def rand[A](nel: NonEmptyList[A], repeat: Int)(using concat: Concatenable[A]): A = {
    def randList[A](nel: NonEmptyList[A], repeat: Int)(using concat: Concatenable[A]): NonEmptyList[A] = {
      if repeat <= 1 then NonEmptyList(nel.toList(Random.nextInt(nel.length)), Nil)
      else NonEmptyList(nel.toList(Random.nextInt(nel.length)), randList(nel, repeat - 1).toList)
    }
    val list = randList(nel, repeat)
    list.tail.foldLeft(list.head)((acc, elem) => concat.++(acc, elem))
  }

  def rand[A](head: A, tail: A*)(using concat: Concatenable[A]): A = {
    val nel = NonEmptyList(head, tail.toList)
    rand(nel, 1)
  }

  def wRand[A](nel: NonEmptyList[A], weights: NonEmptyList[Double], repeat: Int): NonEmptyList[A] = {
    require(nel.length == weights.length, "nel and weights must have same size")
    require(weights.forall(_ >= 0), "weights must be non‑negative")

    // prefix sums: [w1, w1+w2, w1+w2+w3, ...]
    val prefix = weights.toList.scanLeft(0.0)(_ + _).drop(1) // drop the initial 0.0
    val total  = prefix.lastOption.getOrElse(0.0)

    def chooseOne(r: Double): A =
      nel.toList(prefix.indexWhere(_ >= r))

    // generate repeat random choices
    def genOne: A = {
      val r = Random.nextDouble() * total
      chooseOne(r)
    }
    NonEmptyList(genOne, List.fill(repeat - 1)(genOne))
  }

  def randInt(low: Int, high: Int, repeat: Int): Seq[Int] = Seq.fill(repeat)(Random.between(low, high + 1))

  def shuffle[A](seq: Seq[A], repeat: Int): Seq[A] = Random.shuffle(seq).repeat(repeat)
}
