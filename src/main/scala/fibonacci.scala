def run_fib =
  println(fibonacci.take(10).toList)
  println(fibonacci2.take(10).toList)
  println(fibonacci3.take(10).toList)
  println(fibonacci2.drop(15).next)
// use stream to generate fibonacci
def fibonacci: Stream[Int] =
  def fib(a: Int, b: Int): Stream[Int] =
    a #:: fib(b, a + b)
  fib(0, 1)

// unfold function
// In each iteration, the returned tuple of type Option[(A, S)] determines a few things:
//the 1st tuple element of type A is the new element to be added to the resulting sequence
//the 2nd tuple element of type S is the next state value, revealing how the state is being iteratively mutated
//a returned Some((elem, state)) signals a new element being generated whereas a returned None signals the “termination” for the sequence generation operation
def fibonacci2: Iterator[BigInt] =
  Iterator.unfold(BigInt(0), BigInt(1)) { case (a, b) =>
    Some((a, (b, a + b)))
  }
// fibonacci3 is a little different from the previous one fibonacci2
// fibonacci3 starts from 0. (0, 1, 1, 2, 3, 5)
// fibonacci2 starts from 1. (1, 1, 2, 3, 5, 8)
def fibonacci3: Iterator[BigInt] =
  Iterator.unfold(BigInt(0), BigInt(1)) { case (a, b) =>
    Some((b, (b, a + b))) // note b
  }
def fibonacci4: LazyList[BigInt] =
  BigInt(0) #:: BigInt(1) #:: fibonacci4.zip(fibonacci4.tail).map { n =>
    n._1 + n._2
  }

// Elements are memoized; that is, the value of each element is computed at most once.
// Note that the definition of fibs uses val not def.
// The memoization of the LazyList requires us to have somewhere to store the information and a val allows us to do that.
val fibs: LazyList[BigInt] =
  BigInt(0) #:: BigInt(1) #::
    fibs.zip(fibs.tail).map { n =>
      println(s"Adding ${n._1} and ${n._2}")
      n._1 + n._2
    }
//

// We'll start with a silly iteration
def loop(s: String, i: Int, iter: Iterator[Int]): Unit = {
  // Stop after 200,000
  if (i < 200001) {
    if (i % 50000 == 0) println(s + i)
    loop(s, iter.next(), iter)
  }
}

// Our first LazyList definition will be a val definition
val lazylist1: LazyList[Int] = {
  def loop(v: Int): LazyList[Int] = v #:: loop(v + 1)
  loop(0)
}

// Because lazylist1 is a val, everything that the iterator produces is held
// by virtue of the fact that the head of the LazyList is held in lazylist1
def test1_lazy =
  val it1 = lazylist1.iterator
  loop("Iterator1: ", it1.next(), it1)

// We can redefine this LazyList such that all we have is the Iterator left
// and allow the LazyList to be garbage collected as required. Using a def
// to provide the LazyList ensures that no val is holding onto the head as
// is the case with lazylist1
def lazylist2: LazyList[Int] = {
  def loop(v: Int): LazyList[Int] = v #:: loop(v + 1)
  loop(0)
}
def test2_lazy =
  val it2 = lazylist2.iterator
  loop("Iterator2: ", it2.next(), it2)

// And, of course, we don't actually need a LazyList at all for such a simple
// problem. There's no reason to use a LazyList if you don't actually need
// one.
val it3 = new Iterator[Int] {
  var i = -1
  def hasNext = true
  def next(): Int = { i += 1; i }
}
def test3_lazy =
  loop("Iterator3: ", it3.next(), it3)

// add lazy in front of val ?

val fib: LazyList[Int] = {
  def loop(h: Int, n: Int): LazyList[Int] = h #:: loop(n, h + n)
  loop(0, 1)
}
def fibs_test =
  // compted at most once
  fibs.take(5).foreach(println)
  fibs.take(7).foreach(println)

def tailWithSideEffect: LazyList[Nothing] = {
  println("getting empty LazyList")
  LazyList.empty
}
// evaluating the tails content is deferred until the tails empty status, head or tail is evaluated.
def test_tailWithSideEffect =
  val emptyTail = tailWithSideEffect // prints "getting empty LazyList"

  val suspended = 1 #:: tailWithSideEffect // doesn't print anything
  val tail =
    suspended.tail // although the tail is evaluated, *still* nothing is yet printed
  val filtered = tail.filter(_ => false) // still nothing is printed
  val status = filtered.isEmpty // prints "getting empty LazyList"

def zip_test =
  var a = List("a", "b", "c")
  var b = List(1, 2, 3)
  var c = a.zip(b)
  println(c)
  var d = List("x", "y")
  println(a.zip(d))
