sealed trait Fold[Acc, -Val] {
  type Apply[A <: Acc, V <: Val] <: Acc
  def apply[A <: Acc, V <: Val](a: A, v: V): Apply[A, V]
}

sealed trait HList {
  // our Fold function has to accept any type, since we don't know what's in the HList
  type Foldr[Init <: Acc, F <: Fold[Acc, Any], Acc] <: Acc
  def foldr[Init <: Acc, F <: Fold[Acc, Any], Acc](
      init: Init,
      fold: F
  ): Foldr[Init, F, Acc]
}

object HList {
  // TODO: do we need parameters here?
  // tutorial does not have them:
  // https://apocalisp.wordpress.com/2010/07/06/type-level-programming-in-scala-part-6a-heterogeneous-list%C2%A0basics/
  type ::[H, T <: HList] = HCons[H, T]
  // for pattern matching case classes
  // allows us to say a :: b, rather than HCons[a, b]
  // TODO: reference to generated companion object for HCons, so we can use
  // its unapply() method I think?
  val :: : HCons.type = HCons
}

import HList._

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  type Foldr[Init <: Acc, F <: Fold[Acc, Any], Acc] =
    F#Apply[T#Foldr[Init, F, Acc], H]
  def ::[T0](v: T0) = HCons(v, this)
  def foldr[Init <: Acc, F <: Fold[Acc, Any], Acc](
      init: Init,
      fold: F
  ): Foldr[Init, F, Acc] =
    fold.apply[Acc, H](tail.foldr[Init, F, Acc](init, fold), head)
}

sealed trait HNil extends HList {
  type Foldr[Init <: Acc, F <: Fold[Acc, Any], Acc] = Init
  def ::[T](v: T) = HCons(v, this)
  def foldr[Init <: Acc, F <: Fold[Acc, Any], Acc](
      init: Init,
      fold: F
  ): Init = init
}

final object HNil extends HNil

object Main {
  def main(args: Array[String]): Unit = {
    val x: String :: Boolean :: HNil = "str" :: true :: HNil
    x match {
      // these two cases are equivalent
      // case HCons("str", HCons(true, HNil)) => println("yup2")
      case "str" :: true :: _ => println("yup")
      case _                  => println("nope")
    }
  }

}
