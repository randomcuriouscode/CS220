import Lecture1._

class TestSuite extends org.scalatest.FunSuite {
	test("oddNumbers properly defined") {
		assert(oddNumbers == List(1, 3, 5))
	}

	test("sumDouble works") {
		assert(sumDouble(oddNumbers) == 18)
		assert(sumDouble(List(2)) == 4)
		assert(sumDouble(Nil) == 0)
	}

	test("removeZeroes works") {
		val testList = 0 :: 0 :: 0 :: 1 :: 2 :: Nil
		assert(testList.contains(0))
		assert(!(removeZeroes(testList).contains(0)))
		assert(!(removeZeroes(List(1,0,0)).contains(0)))
		assert(!(removeZeroes(Nil).contains(0)))
	}

	test("countEvens works"){
		val testList = 0 :: 0 :: 0 :: 2 :: 1 :: Nil

		assert(testList.length == 5)
		assert(countEvens(testList) == 4)
		assert(countEvens(Nil) == 0)
		assert(countEvens(List(1,1,1,1,2)) == 1)
	}

	test("removeAlternating works"){
		val testList = "0" :: "1" :: "2" :: "3" :: "4" :: "5" :: Nil
		val testList2 = "0" :: "1" :: Nil
		val testList3 = "0" :: "1" :: "2" :: Nil

		assert(removeAlternating(testList) == List("0", "2", "4"))
		removeAlternating(testList).foreach(x => println("testList: " + x));
		assert(removeAlternating(testList2) == List("0"))
		assert(removeAlternating(testList3) == List("0", "2"))
	}

	test("isAscending works"){
		val testList = List(0,0,1,3,4)
		val testList2 = List(1,0,2,3,4)

		assert(isAscending(testList))
		assert(!isAscending(testList2))
	}

	test("addSub works"){
		val testList = List(5,1,5,1,5)
		assert(addSub(testList) == 13)
	}

	test("alternate works"){
		assert(alternate(List(1,3,5), List(2,4,6)) == List(1,2,3,4,5,6))
		assert(alternate(List(1), List(2)) == List(1,2))
	}

	test("fromTo works"){
		assert(fromTo(2,5) == List(2,3,4))
		assert(fromTo(2,3) == List(2))
		assert(fromTo(2,2) == Nil)
	}

	test("insertOrdered works"){
		assert(insertOrdered(4, List(1,2,3,5)) == List(1,2,3,4,5))
		assert(insertOrdered(1, List(1,2,3,5)) == List(1,1,2,3,5))
		assert(insertOrdered(1, Nil) == List(1))
	}

	test("sort works"){
		assert(sort(List(5,4,3,2,1)) == List(1,2,3,4,5))
		assert(sort(List(5,5,4,3,2,1)) == List(1,2,3,4,5,5))
		assert(sort(List(2,1)) == List(1,2))
	}

}