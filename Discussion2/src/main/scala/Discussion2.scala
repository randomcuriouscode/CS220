object Discussion2 {
	sealed trait Bird
	case class Duck() extends Bird
	case class Goose() extends Bird

	def map[A,B](f : A => B, xs: List[A]) : List[B] = xs match {
		case Nil => Nil
		case head :: tail => f(head) :: map[A,B](f, tail)
	}

	val sumthem = (acc : Int, bird: Bird) => bird match {
		case Duck() => acc + 1
		case Goose() => acc + 10
		case _ => acc
		}
		
	val convertThem = (bird: Bird) => bird match {
		case Duck() => "dog food"
		case Goose() => "pate"
		case _ =>  "bird? error"
		}

	def fold[A,B](acc : B, f: (B,A) => B, xs: List[A]) : B = {
		xs match { 
			case Nil => acc
			case h:: tail => fold(f(acc,h), f, tail)
		}
	}
}