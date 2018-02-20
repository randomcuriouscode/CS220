import Regexes._

class TestSuite extends org.scalatest.FunSuite {

	test (" The Regexes object must be defined ") {
		val regexes : hw.regex.RegexLike = Regexes
}

	test("time match works"){
		val t = time.pattern
		assert(t.matcher("00:00").matches)
		assert(t.matcher("23:59").matches)
	}

	test("comment match works"){
		val c = comment.pattern
		assert(c.matcher("/**/").matches)
		assert(c.matcher("/*l*/").matches)
		assert(c.matcher("/*lllllllgggfdasfahguwaeghauefj*/").matches)
	}

}