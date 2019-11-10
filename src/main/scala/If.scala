// if implicitly[A, B] compiles, A :=: B
sealed trait TypeEq[A, B]
object TypeEq {
  implicit def typeEq[A]: TypeEq[A, A] = new TypeEq[A, A] {}
}

object If {

  sealed trait Bool {
    type If[T <: Up, F <: Up, Up] <: Up
  }

  object Bool {
    type &&[A <: Bool, B <: Bool] = A#If[B, False, Bool]
    type ||[A <: Bool, B <: Bool] = A#If[True, B, Bool]
    type Not[A <: Bool] = A#If[False, True, Bool]
  }
  sealed trait True extends Bool {
    type If[T <: Up, F <: Up, Up] = T
  }
  sealed trait False extends Bool {
    type If[T <: Up, F <: Up, Up] = F
  }

  type MaybeIntMaybeBool[A <: Bool] = A#If[Int, Boolean, AnyVal]

  def main(args: Array[String]): Unit = {
    val i: MaybeIntMaybeBool[True] = 1 // this is an int!
    // =:= is the built in TypeEq
    val equalTypes1: =:=[Int, Int] = implicitly[MaybeIntMaybeBool[True] =:= Int]
    val equalTypes2: TypeEq[Int, Int] =
      implicitly[TypeEq[MaybeIntMaybeBool[True], Int]]
    // This does not compile
    // val notEqualTypes = implicitly[TypeEq[MaybeIntMaybeBool[False], Int]]
    implicitly[Bool.&&[True, False] =:= False]
    implicitly[Bool.&&[True, False] =:= False]
    implicitly[Bool.&&[True, False] =:= False]
    println("Hello, world!")
  }
}
