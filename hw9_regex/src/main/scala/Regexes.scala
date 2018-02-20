import scala.util.matching.Regex
object Regexes extends hw.regex.RegexLike {
	def notAlphanumeric : Regex = """(\W)*""".r
	def time : Regex = """(2[0-3]|[0-1][0-9]):[0-5][0-9]""".r
	def phone : Regex = """\([1-9][0-9]{2}\) [0-9]{3}-[0-9]{4}""".r
	def zip : Regex = """(\d{5}-\d{4})|\d{5}""".r
	def comment : Regex = "\\/\\*.*\\*\\/".r
	def numberPhrase : Regex = """(ninety-nine)|(\.\.\.)|(twenty-one)|(twenty-three)|(twenty)""".r
	def roman : Regex = """(X){0,3}((IV)|(V)){0,1}(I|V|X){0,3}""".r
	def date : Regex = """^(((19|20)([2468][048]|[13579][26]|0[48])|2000)[/-]02[/-]29|((19|20)[0-9]{2}[/-](0[4678]|1[02])[/-](0[1-9]|[12][0-9]|30)|(19|20)[0-9]{2}[/-](0[1359]|11)[/-](0[1-9]|[12][0-9]|3[01])|(19|20)[0-9]{2}[/-]02[/-](0[1-9]|1[0-9]|2[0-8])))$""".r
	def evenParity : Regex = """(([02468]*[13579]){2})*[02468]*""".r
}