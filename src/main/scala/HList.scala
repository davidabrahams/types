trait Fold[Acc, -Val] {
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
  type Foldl[Init <: Acc, F <: Fold[Acc, Any], Acc] <: Acc
  def foldl[Init <: Acc, F <: Fold[Acc, Any], Acc](
      init: Init,
      fold: F
  ): Foldl[Init, F, Acc]
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
  // we could just let type inference figure out the type of reverse()
  // but it's more fun to define a type level Reverse function, and ensure
  // our implementation is correct
  type Reverse[A <: HList] = A#Foldl[HNil, ConsFold.type, HList]
  implicit class HListOps[H <: HList](val h: H) extends AnyVal {
    def reverse: Reverse[H] =
      h.foldl[HNil, ConsFold.type, HList](HNil, ConsFold)
  }
}

import HList._

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  def ::[T0](v: T0) = HCons(v, this)

  type Foldr[Init <: Acc, F <: Fold[Acc, Any], Acc] =
    F#Apply[T#Foldr[Init, F, Acc], H]
  def foldr[Init <: Acc, F <: Fold[Acc, Any], Acc](
      init: Init,
      fold: F
  ): Foldr[Init, F, Acc] =
    fold(tail.foldr[Init, F, Acc](init, fold), head)
  // to implement a fold left, we combine H into the "Init" for the next recursion
  type Foldl[Init <: Acc, F <: Fold[Acc, Any], Acc] =
    T#Foldl[F#Apply[Init, H], F, Acc]
  def foldl[Init <: Acc, F <: Fold[Acc, Any], Acc](
      init: Init,
      fold: F
  ) = tail.foldl(fold(init, head), fold)
}

sealed trait HNil extends HList {
  def ::[T](v: T) = HCons(v, this)
  type Foldr[Init <: Acc, F <: Fold[Acc, Any], Acc] = Init
  def foldr[Init <: Acc, F <: Fold[Acc, Any], Acc](
      init: Init,
      fold: F
  ): Init = init
  type Foldl[Init <: Acc, F <: Fold[Acc, Any], Acc] = Init
  def foldl[Init <: Acc, F <: Fold[Acc, Any], Acc](
      init: Init,
      fold: F
  ): Init = init
}

object HNil extends HNil {}

// A Fold which simply Cons the new element on the front of the accumulator
object ConsFold extends Fold[HList, Any] {
  type Apply[H <: HList, N <: Any] = N :: H

  // used later for value-level implementations
  def apply[B <: HList, A](b: B, a: A) = HCons(a, b)
}

object Main {
  def main(args: Array[String]): Unit = {
    val x: String :: Boolean :: HNil = "str" :: true :: HNil
    val y: Boolean :: String :: HNil = x.reverse
    x match {
      // these two cases are equivalent
      // case HCons("str", HCons(true, HNil)) => println("yup2")
      case "str" :: true :: _ => println("yup")
      case _                  => println("nope")
    }
    y match {
      // these two cases are equivalent
      // case HCons("str", HCons(true, HNil)) => println("yup2")
      case true :: "str" :: _ => println("yup")
      case _                  => println("nope")
    }
    type Foo = String :: Boolean :: HNil
    type Bar = Boolean :: String :: HNil
    // test our reverse implementation is correct
    implicitly[Reverse[Foo] =:= Bar]
  }
}
