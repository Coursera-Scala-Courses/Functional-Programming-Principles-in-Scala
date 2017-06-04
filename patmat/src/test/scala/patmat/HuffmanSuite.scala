package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("encode a very short text") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0,1))
    }
  }

  test("encode another very short text") {
    new TestTrees {
      assert(encode(t1)("abba".toList) === List(0,1,1,0))
    }
  }

  test("decode a very short text") {
    new TestTrees {
      assert(decode(t1, List(0,1)) === "ab".toList)
    }
  }

  test("decode another very short text") {
    new TestTrees {
      assert(decode(t1, List(0,1,0)) === "aba".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("return the code bits from code table for a given char") {
    new TestTrees {
      assert( codeBits(List(('b',List(1)),('a',List(0))))('a') === List(0))
    }
  }

  test("convert the code tree into code table for a given char") {
    new TestTrees {
      assert( convert(t2) === List(('a',List(0,0)),('b',List(0,1)),('d',List(1))))
    }
  }

  test("quick encode a very short text") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0,1))
    }
  }

  test("quick encode another very short text") {
    new TestTrees {
      assert(quickEncode(t1)("abba".toList) === List(0,1,1,0))
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
