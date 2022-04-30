// use for loop to seperate a list by a element value, just like string
def split_string_list(
    ls: List[String],
    delimiter: String
): List[List[String]] = {
  var result: List[List[String]] = List()
  var temp: List[String] = List()
  for (s <- ls) {
    if (s != delimiter) {
      temp = temp :+ s
    } else {
      if !temp.isEmpty then result = result :+ temp
      temp = List() // reset temp
    }
  }
  result
}

def split_string_list1(
    ls: List[String],
    delimiter: String
): List[List[String]] = {
  var result: List[List[String]] = List()
  var temp: List[String] = List()
  // use foreach to iterate with side effect
  ls.foreach { x =>
    if (x != delimiter) {
      temp = temp :+ x
    } else {
      if !temp.isEmpty then result = result :+ temp
      temp = List() // reset temp
    }
  }
  result
}

def split_string_list2(
    ls: List[String],
    delimiter: String
): List[List[String]] =
  List.unfold(ls) {
    case Nil                        => None
    case xs if xs.head != delimiter => Some(xs.span(_ != delimiter))
    case _ :: t                     => Some(t.span(_ != delimiter))
  }
def split_string_list3[T](
    ls: List[T],
    delimiter: T
): Seq[Seq[T]] =
  ls.foldLeft(Seq(Seq.empty[T])) { (acc, i) =>
    if i == delimiter then acc :+ Seq.empty
    else acc.init :+ (acc.last :+ i)
  }

import collection.mutable.ListBuffer
def splitBySeparator[T](ls: Seq[T], sep: T): Seq[Seq[T]] = {
  val b = ListBuffer(ListBuffer[T]())
  ls.foreach { e =>
    if e != sep then b.last += e
    else b += ListBuffer[T]()
  }
  b.map(_.toSeq).toSeq
}

def split_int_list =
  val l =
    Seq(1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 5)

  val res = l.foldLeft(Seq(Seq.empty[Int])) { (acc, i) =>
    if (i == 9) acc :+ Seq.empty
    else acc.init :+ (acc.last :+ i)
  }
  println(res)

// scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

def encodeDirect[A](l: List[A]): List[(Int, A)] = {
  def _encodeDirect(res: List[(Int, A)], rem: List[A]): List[(Int, A)] =
    rem match {
      case Nil => res
      case ls => {
        val (s, r) = rem span { _ == rem.head }
        _encodeDirect(res ::: List((s.length, s.head)), r)
      }
    }
  _encodeDirect(List(), l)
}

// scala> Stream.from(1).partition(_ % 2 == 0)
//val res0: (Stream[Int], Stream[Int]) = (Stream(2, <not computed>),Stream(1, <not computed>))
// Stream.from(1).span(_ % 2 == 0)
//val res1: (Stream[Int], Stream[Int]) = (Stream(),Stream(1, <not computed>))
// use partition to get odd and even numbers
def partition_odd_even(start: Int, end: Int): (List[Int], List[Int]) =
  val (odd, even) =
    LazyList.from(start).partition(_ % 2 != 0)
  (odd.takeWhile(_ <= end).toList, even.takeWhile(_ <= end).toList)
def partition_odd_even2(start: Int, end: Int): (List[Int], List[Int]) =
  // range is end inclusive
  val (odd, even) = LazyList.range(start, end + 1).partition(_ % 2 != 0)
  (odd.toList, even.toList)
//stream elements are not computed until they are needed, called lazily
// use a collection that works like a List but invokes its transformer methods (map, filter, etc.) lazily.

//
/*
scala> val x = List(15, 10, 5, 8, 20, 12)
x: List[Int] = List(15, 10, 5, 8, 20, 12)


scala> val y = x.groupBy(_ > 10)
y: Map[Boolean,List[Int]] = Map(false -> List(10, 5, 8), true -> List(15, 20, 12))

scala> val y = x.partition(_ > 10)
y: (List[Int], List[Int]) = (List(15, 20, 12), List(10, 5, 8))

The span method returns a Tuple2 based on your predicate p, consisting of “the longest prefix of this list whose elements all satisfy p, and the rest of this list.”
scala> val y = x.span(_ < 20)
y: (List[Int], List[Int]) = (List(15, 10, 5, 8), List(20, 12))

scala> val y = x.splitAt(2)
y: (List[Int], List[Int]) = (List(15, 10), List(5, 8, 20, 12))
 */
