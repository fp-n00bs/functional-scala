package net.dp.essentials

import java.time.LocalDateTime

import net.dp.essentials.higher_kinded.ListCollectionLike
import net.dp.essentials.higher_order._
import net.dp.essentials.poly_functions._
import net.dp.essentials.typeclasses.{ CollectionSyntax, FilterableSyntax }
import net.dp.essentials.types.{ BankAccount2, SavingsAccount }

object Notes extends App {

  override def main(args: Array[String]): Unit = {

    //Functions -------------------------------------------------------------
    BankAccount2
      .create(1L, 10.0, SavingsAccount, LocalDateTime.now())
      .ensuring(_.isDefined)

    BankAccount2
      .create(-1L, 10.0, SavingsAccount, LocalDateTime.now())
      .ensuring(_.isEmpty)

    // functions.printer2(x => println(x), (_, _) => ()) //Todo

    //Higher order functions -------------------------------------------------
    fanout(minus, plus)(10)
      .ensuring(res => res == (9, 11))

    cross(minus, plus)(10, 20)
      .ensuring(res => res == (9, 21))

    either(minus, plus)(Left(10))
      .ensuring(res => res == 9)

    either(minus, plus)(Right(10))
      .ensuring(res => res == 11)

    choice(minus, plus)(Left(10))
      .ensuring(res => res == (Left(9)))

    choice(minus, plus)(Right(10))
      .ensuring(res => res == (Right(11)))

    compose(minus, plus)(10)
      .ensuring(res => res == 10)

    //alt() Todo Hur kör man denna?

    //Polymorphic functions --------------------------------------------------
    repeat[String](10)("", _ + "*")
      .ensuring(res => res.equals("**********"))

    groupBy1(TestData, ByDate)(Reducer)
      .ensuring(res => res.equals(ExpectedResults))

    groupBy2(TestData, ByDate)(Reducer)
      .ensuring(res => res.equals(ExpectedResults))

    //Higher kinded types ------------------------------------------------------
    ListCollectionLike
      .append(List(1, 2, 3), List(1, 2, 3))
      .filter(x => x == 2)
      .ensuring(res => res.size == 2)

    //Implicit / Type classes -------------------------------------------------------------
    List(1, 2, 3)
      .filterWith(_ == 2)
      .ensuring(res => res.length == 1)

    List(1, 2, 3).uncons
      .ensuring(res => res == Some((1, List(2, 3))))

  }
  private def plus = { (x: Int) => x + 1}
  private def minus = { (y: Int) => y - 1}

}
