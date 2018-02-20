import Discussion2._

class TestSuite extends org.scalatest.FunSuite {

	test("Duck duck goose"){
		val testL = List(Duck(), Duck(), Goose())

		assert(map[Bird, String](convertThem, testL) == List("dog food", "dog food", "pate"))
	}

	test("money money"){
		val testL = List(Duck(), Duck(), Goose())
		val testL2 = List(Goose(), Goose(), Goose(), Goose(), Duck(), Goose())
		val testL3 = List()

		assert(fold(0, sumthem , testL) == 12)
		assert(fold(0, sumthem, testL2) == 51)
		assert(fold(0, sumthem, testL3) == 0)

	}
}