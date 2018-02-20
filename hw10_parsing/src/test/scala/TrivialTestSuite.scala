import hw.parsing._
import scala.util.parsing.combinator._
import ArithEval._
import ArithParser._
import ArithPrinter._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, BooleanOperators}

class TrivialTestSuite extends org.scalatest.FunSuite{

	test("several objects must be defined"){
		val parser: hw.parsing.ArithParserLike = ArithParser
		val printer: hw.parsing.ArithPrinterLike = ArithPrinter
		val eval: hw.parsing.ArithEvalLike = ArithEval
	}

}