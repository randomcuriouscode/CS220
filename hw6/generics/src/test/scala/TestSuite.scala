import ListFunctions._
import hw.generics._


case class MyInt(value: Int) extends hw.generics.Ordered[MyInt] {
	def compare(other: MyInt) : Ordering = {
		if (value < other.value) LT
		else if(value > other.value) GT
		else EQ
	}
}

class TestSuite extends org.scalatest.FunSuite {
	val balanced_nodes = Node[Int](Node(Leaf(), 1, Leaf()), 5, Node(Leaf(), 10, Leaf()))

	test("Sort works trivially(list and bintree)"){
		val unsorted : BinTree[MyInt] = Node[MyInt](Node(Leaf(), MyInt(10), Leaf()), MyInt(5), Node(Leaf(), MyInt(1), Leaf()))
		val actual_unsort : MyList[MyInt] = Cons(MyInt(5),Cons(MyInt(4),Cons(MyInt(3), Empty[MyInt])))
		assert(sort[MyInt, BinTree[MyInt]](unsorted) == Node(Leaf(),MyInt(1),Node(Leaf(),MyInt(5),Node(Leaf(),MyInt(10),Leaf()))))
		assert(sort[MyInt, MyList[MyInt]](actual_unsort) == Cons(MyInt(3),Cons(MyInt(4),Cons(MyInt(5), Empty[MyInt]))))
	}

	test("Sort works complicatedly"){
		val actual_unsort : MyList[MyInt] = Cons(MyInt(5),Cons(MyInt(4),Cons(MyInt(3), Empty[MyInt])))
		assert(sort[MyInt, MyList[MyInt]](append[MyInt, MyList[MyInt]](actual_unsort, actual_unsort)) == 
			Cons(MyInt(3),Cons(MyInt(3),Cons(MyInt(4), Cons(MyInt(4),Cons(MyInt(5),Cons(MyInt(5), Empty[MyInt])))))))		
		
	}

	test("Filter works trivially"){
		assert(filter[Int, BinTree[Int]](a => a == 5, balanced_nodes) == Node[Int](Leaf(), 5, Leaf()))
	}

	test("Append works trivially"){
		assert(append[Int, BinTree[Int]](balanced_nodes, balanced_nodes) == balanced_nodes.cons(10).cons(5).cons(1))
	}

	
}