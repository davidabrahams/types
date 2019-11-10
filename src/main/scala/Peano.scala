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
    type FoldR[Init <: Up, FoldFunc[Up, Nat] <: Up, Up] <: Up
  }

  sealed trait FoldFunc[-Acc, +Val] {
    type Apply[Acc, Val] <: Val
  }

  sealed trait _0 extends Nat {
    type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = IfZero
    type Compare[N <: Nat] = N#Match[ConstLT, EQ, Comparison]
    // base case
    type FoldR[Init <: Up, FoldFunc[Up, Nat] <: Up, Up] = Init
  }
  sealed trait Succ[N <: Nat] extends Nat {
    type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = NonZero[N]
    // if M is NonZero, we compare (M-1) to N. Otherwise return GT
    type Compare[M <: Nat] = M#Match[N#Compare, GT, Comparison]
    type FoldR[Init <: Up, Func <: FoldFunc[Up, Nat], Up] =
      Func#Apply[N#FoldR[Init, Func, Up], Succ[N]]
  }

  // type Increment[N <: Nat, M <: Nat] = Succ[N]
  // type Add[N <: Nat, M <: Nat] = M#FoldR[N, Increment, Nat]

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
    println("Success!")
  }
}
