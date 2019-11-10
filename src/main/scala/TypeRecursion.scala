import cats.implicits._

object TypeRecursion extends App {
  val foo: Foo = new FooImpl()
  val bar: Foo = new Foo {
    def next = this
    def x(f: Foo) = 1
  }
  println("starting infinite loop")
  val i: Int = foo.x(bar)
  println("Hello, World!")
  println(i)
}

trait Foo {
  def next: Foo
  def x(f: Foo): Int
}

class FooImpl extends Foo {
  def next: Foo = this
  def x(f: Foo): Int = f.x(next)
}

// define the abstract types and bounds
trait Recurse {
  type Next <: Recurse
  // type level function
  // X : Recurse => Int
  type X[R <: Recurse] <: Int
}
// implementation
trait RecurseA extends Recurse {
  type Next = RecurseA
  // this is the implementation
  type X[R <: Recurse] = R#X[R#Next]
}

trait RecurseB extends Recurse {
  type Next = RecurseB
  // this is the implementation
  type X[R <: Recurse] = Int
}

object Recurse {
  // infinite loop
  // type C = RecurseA#X[RecurseA]
  type D = RecurseA#X[RecurseB]
  val i: D = 1
  val j: Int = i
}
