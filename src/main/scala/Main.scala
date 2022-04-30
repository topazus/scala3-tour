@main def hello: Unit =
  println(split_string_list(List("BREAK", "Apple", "BREAK"), "BREAK"))
  println(split_string_list2(List("BREAK", "Apple", "BREAK"), "BREAK"))
  println(split_string_list3(List("BREAK", "Apple", "BREAK"), "BREAK"))
  // println()
  println(splitBySeparator(List("a", "b", "BREAK", "Apple", "BREAK"), "BREAK"))
  println(splitBySeparator(List("BREAK", "Apple", "BREAK"), "BREAK"))
  println("a:::".split(":").toList)
def msg = "I was compiled by Scala 3. :)"

// read files to string
def read_file_to_list(): List[String] =
  val f = "/proc/cpuinfo"
  val lines = io.Source
    .fromFile(f)
    .getLines
    // filter lines that predicate is true
    .filter(x => x != "")
    .toList
  lines

def read_file_to_list2: List[List[String]] =
  println("Hello, world!")
  val f = "/proc/cpuinfo"
  val lines = io.Source
    .fromFile(f)
    .getLines
    .toList
  val res = List.unfold(lines) {
    case Nil =>
      None
    case l if l.head == "\n" =>
      Some(l.span(_ == "\n"))
    case _ :: t =>
      Some(t.span(_ != "\n"))
  }
  res

def fold_and_unfold =
  //
  val a = Iterator(1 to 10: _*).fold(10)(_ + _)
  println(a)
def countDown(from: Int): Iterator[Int] =
  Iterator.iterate(from)(_ - 1)
def countDown2(from: Int): Iterator[Int] =
  Iterator.iterate(from) { x =>
    x - 1
  }
// unfold the result of a function as long as it returns a Some
def countDown4(from: Int): Iterator[Int] =
  Iterator.unfold(from) { count =>
    if count == 0 then None
    else Some((count, count - 1))
  }
