import hw.parsing._
import scala.util.parsing.combinator._

//1. Implement ArithEval. This is a simple recursive function.
object ArithEval extends ArithEvalLike{

	def eval(e: Expr): Double = e match {
		case a : Add => eval(a.e1) + eval(a.e2)
		case a : Div => eval(a.e1) / eval(a.e2)
		case a : Exponent => Math.pow(eval(a.e1), eval(a.e2))
		case a : Mul => eval(a.e1) * eval(a.e2)
		case a : Num => a.n
		case a : Sub => eval(a.e1) - eval(a.e2)
	}

}

//2. Implement ArithParser by translating the grammar provided above to Scala's parser combinators.
object ArithParser extends ArithParserLike{

	// number : PackratParser [ Double ] is defined in ArithParserLike
	
	lazy val atom: PackratParser[Expr] = number ^^ Num | (("("  ~> expr) <~ ")") 

	lazy val exponent: PackratParser[Expr] = (exponent ~ ("^" ~> atom)) ^^ { case a ~ b => Exponent(a,b) } | atom

	lazy val add: PackratParser[Expr] =  ((mul <~ "+") ~ add) ^^ { case a ~ b => Add(a,b)} | ((mul <~ "-") ~ add) ^^ { case a ~ b => Sub(a,b)} | mul
 
	lazy val mul: PackratParser[Expr] = ((exponent <~ "*") ~ mul) ^^ { case a ~ b => Mul(a,b)} | ((exponent <~ "/") ~ mul) ^^ { case a ~ b => Div(a,b)} | exponent 

	lazy val expr: PackratParser[Expr] = add

}

//3. Implement ArithPrinter. We suggest using ScalaCheck to test these functions.
//(You'll have to define generators as part of your test suite.)
object ArithPrinter extends ArithPrinterLike {

	def print (e: Expr): String = {
		e match {
			case a : Add => "( " + print(a.e1) + " + " + print(a.e2) + " )"
			case a : Div => "( " + print(a.e1) + " / " + print(a.e2) + " )"
			case a : Exponent => "( " + print(a.e1) + " ^ " + print(a.e2) + " )"
			case a : Mul => "( " + print(a.e1) + " * " + print(a.e2) + " )"
			case a : Num => a.n.toString
			case a : Sub => "( " + print(a.e1) + " - " + print(a.e2) + " )"
		}
	}

}