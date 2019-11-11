object Peano {

  type ConstFalse[A] = False
  type ConstLT[A] = LT
  sealed trait Nat {
    // NonZero is type constructor, which produces a type given N - 1
    type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up
    // a type level function to compare Nats
    type Compare[N <: Nat] <: Comparison
    // a type level function that folds over ints
    // we must provide a two argument type constructor to fold each int
    // into Up
    type FoldR[Init <: Acc, Func <: FoldFunc[Acc, Nat], Acc] <: Acc
  }

  // a little weird, FoldFunc[A, B] is a type with a type-function (A, B) => B
  // FoldFunc is contravarient Val, as it is the input to the function
  // FoldFunc is invariant in Acc, because Acc is both the input type
  // (requiring contravariance) and the output type (requiring covariance)
  sealed trait FoldFunc[Acc, -Val] {
    type Apply[A <: Acc, V <: Val] <: Acc
  }

  sealed trait _0 extends Nat {
    type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = IfZero
    type Compare[N <: Nat] = N#Match[ConstLT, EQ, Comparison]
    // base case
    type FoldR[Init <: Acc, Func <: FoldFunc[Acc, Nat], Acc] = Init
  }
  sealed trait Succ[N <: Nat] extends Nat {
    type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = NonZero[N]
    // Compare is comparing M to N+1
    // if M is Zero, return GT
    // if M is NonZero, we compare (M-1) to N.
    type Compare[M <: Nat] = M#Match[N#Compare, GT, Comparison]
    type FoldR[Init <: Acc, Func <: FoldFunc[Acc, Nat], Acc] =
      Func#Apply[N#FoldR[Init, Func, Acc], Succ[N]]
  }

  type Increment = FoldFunc[Nat, Nat] {
    type Apply[A <: Nat, V <: Nat] = Succ[A]
  }
  type Add[A <: Nat, V <: Nat] = V#FoldR[A, Increment, Nat]
  type AddBy[By <: Nat] = FoldFunc[Nat, Nat] {
    type Apply[A <: Nat, V <: Nat] = Add[A, By]
  }
  type Mult = FoldFunc[Nat, Nat] {
    type Apply[A <: Nat, V <: Nat] = V#FoldR[_0, AddBy[A], Nat]
  }
  // fact(a) == 1 * 2 * 3 * ... * a
  type Fact[V <: Nat] = V#FoldR[_1, Mult, Nat]

  // the comparison type indicates whether on type is less than another type, and can produce
  // a type via #Match
  sealed trait Comparison {
    type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] <: Up

    type gt = Match[False, False, True, Bool]
    type ge = Match[False, True, True, Bool]
    type eq = Match[False, True, False, Bool]
    type le = Match[True, True, False, Bool]
    type lt = Match[True, False, False, Bool]
  }

  sealed trait GT extends Comparison {
    type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfGT
  }
  sealed trait LT extends Comparison {
    type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfLT
  }
  sealed trait EQ extends Comparison {
    type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfEQ
  }

  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  type _11 = Succ[_10]
  type _12 = Succ[_11]

  // given a natural number type, we can recursively produce that same type using Match
  type Id[A <: Nat] = A#Match[Succ, _0, Nat]

  def main(args: Array[String]): Unit = {
    implicitly[Id[_0] =:= _0]
    implicitly[Id[_5] =:= _5] // this compiles! We can prove that these types are the same
    implicitly[_0#Compare[_0]#eq =:= True]
    implicitly[_0#Compare[_1]#eq =:= False]
    implicitly[_3#Compare[_5]#lt =:= True]
    implicitly[_5#Compare[_3]#lt =:= False]
    implicitly[_3#Compare[_3]#le =:= True]
    implicitly[Add[_1, _2] =:= _3]
    implicitly[Add[_1, _4] =:= _5]
    implicitly[Add[_3, _4] =:= _7]
    implicitly[Mult#Apply[_1, _4] =:= _4]
    implicitly[Mult#Apply[_2, _3] =:= _6]
    implicitly[Mult#Apply[_3, _3] =:= _9]
    implicitly[Mult#Apply[_3, _3] =:= _9]
    implicitly[Fact[_0] =:= _1]
    implicitly[Fact[_1] =:= _1]
    implicitly[Fact[_2] =:= _2]
    implicitly[Fact[_3] =:= _6]
    println("Success!")
  }
}
