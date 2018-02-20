import Homework2._

class TestSuite extends org.scalatest.FunSuite {
	test("map2 with add works") {
		def add(x: Int, y: Int): Int = x + y
		assert(map2(add, List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
		assert(map2(add, List(1), List(4)) == List(5))
	}
	
	test("map2 with sub works") {
		def sub(x: Int, y: Int): Int = x - y
		assert(map2(sub, List(1, 2, 3), List(4, 5, 6)) == List(-3, -3, -3))
	}

	test("zip works") {
		assert(zip(List(1, 2, 3), List(4, 5, 6)) == List((1,4), (2, 5), (3, 6)))
		assert(zip(List("George", "Teddy"), List("Washington", "Roosevelt")) == 
			List(("George", "Washington"), ("Teddy", "Roosevelt")))
		assert(zip(List("George", "Teddy"), List("Washington")) == List(("George", "Washington")))
	}

	test("flatten works") {
		assert(flatten(List(List(), List(3, 4))) == List(3, 4))
		assert(flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
		assert(flatten(List(List(1, 2, 3, 4, 5), List(6, 7))) == List(1, 2, 3, 4, 5, 6, 7))
		assert(flatten(List(List(1, 2), List(3, 4, 5, 6, 7))) == List(1, 2, 3, 4, 5, 6, 7))
	}

	test("flatten3 works") {
		assert(flatten3(List(List(List(1,2), List(3,4)))) == List(1,2,3,4))
		assert(flatten3(List(List(List(1, 2, 3, 4, 5), List(6, 7)))) == List(1, 2, 3, 4, 5, 6, 7))
		assert(flatten3(List(List(List(1, 2), List(3, 4, 5, 6, 7)))) == List(1, 2, 3, 4, 5, 6, 7))
		assert(flatten3(List(List(List(1,2,3),List(4,5,6)), List(List(7,8,9),List(10,11,12)))) == List(1,2,3,4,5,6,7,8,9,10,11,12))
	}
	test("buildList works") {
		def f(x: Int) = x
		assert(buildList(10, f) == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
		assert(buildList(0, f) == Nil)
		assert(buildList(1, f) == (List(0)))
	}

	test("mapList test") {
		def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
		def g(n: Int) : List[Int] = List(n * n)
		assert(mapList(List(1, 2, 3), f) == List(1, 2, 2, 3, 3, 3))
		assert(mapList(List(1, 2, 3), g) == List(1, 4, 9))
	}

	def isEven(x: Int): Boolean = x % 2 == 0

	test("partition test other") {
		assert(partition(isEven, List(6,5,4,3,2,1)) == (List(6,4,2), List(5,3,1)))
		assert(partition((x: Int) => x < 5, List(1,2,3,4,5,6)) == (List(1,2,3,4),List(5,6)))
	}

	test("partition test 1") {
		assert(partition(isEven, List(1,2,3,4,5,6)) == (List(2,4,6), List(1,3,5)))
	}
	
	test("partition test 2") {
		assert(partition(isEven, List(2,4,6)) == (List(2,4,6), Nil))
	}

	test("partition test 3") {
		assert(partition(isEven, List(1,3,5)) == (Nil, List(1,3,5)))
	}
}