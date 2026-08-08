//> using scala 3.3.7
//> using dep "org.typelevel::cats-core:2.13.0"

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.all.*

val ok: Validated[String, Int] =
  42.valid

val bad: Validated[String, Int] =
  "Oops".invalid

println(ok)
println(bad)

val a = 10.validNec[String]
val b = 20.validNec[String]
val c = "first error".invalidNec[Int]
val d = "second error".invalidNec[Int]

println((a, b).mapN(_ + _))
println((a, c).mapN(_ + _))
println((c, d).mapN(_ + _))
