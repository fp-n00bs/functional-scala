package net.dp.abstractions
import scalaz._
import Scalaz._
import net.dp.abstractions.algebra.NotEmpty
import net.dp.abstractions.functor.{ zip, ListToOption, OptionToList }

object Notes extends App {

  override def main(args: Array[String]): Unit = {

    //Semigroup -------------------------------------------------------------
    (NotEmpty(1, None) |+| NotEmpty(2, None))
      .ensuring(res => res == algebra.NotEmpty(3, None))

    //Monoid -------------------------------------------------------------
    val map1 = Map(1 -> "Ett", 2 -> "Två")
    val map2 = Map(1 -> "Ett", 2 -> "Två")
    (map1 |+| map2)
      .ensuring(res => res == Map(1 -> "EttEtt", 2 -> "TvåTvå"))

    //Functor -------------------------------------------------------------

    //Todo Hur ska denna anropas?
    //FunctorProduct(List(1, 2, 3), List(1, 2, 3))

    ListToOption(List(1, 2, 3))
      .ensuring(res => res == Some(1))

    OptionToList(Some(2))
      .ensuring(res => res == List(2))

    OptionToList(None)
      .ensuring(res => res == List())

    functor
      .Zip[Option]
      .zip(Some(1), Some(2))
      .ensuring(res => res == Some((1, 2)))

    zip(Option(3), Option("foo"))
      .ensuring(res => res == Option((3, "foo")))

  }
}
