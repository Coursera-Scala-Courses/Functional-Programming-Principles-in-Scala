package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val s = union(singletonSet(2), singletonSet(1))
  printSet(s)
  printSet(map(s, x => x*x*x))
}
