import PathImplicits._
import DateImplicits._
import java.nio.file.Paths
import java.time.LocalDate
import java.time.Month
import java.nio.file.Files

class TestSuite extends org.scalatest.FunSuite {

	val constant_path0 = Paths.get("usr", "bin")
	val constant_path1 = Paths.get("usr", "bin", "scala")
	val constant_path2 = Paths.get("usr", "local", "bin", "scala")

	test("string to string path works"){
		assert(("usr" / "bin") == constant_path0) //concat string to string
		assert((("usr" / "bin") / "scala") == constant_path1) //concat string to path
		assert(("usr" / ("bin" / "scala")) == constant_path1) //concat path to string

		val a = "usr" / "local"
		val b = "bin" / "scala"

		assert((a / b) == constant_path2) //concat path to path
	}

	test("file read write append works"){
		val testfile = "." / "test"
		Files.deleteIfExists(testfile)

		testfile.write("test")
		assert(testfile.read == "test")
		testfile.append("again")
		assert(testfile.read == "testagain")
		
		Files.deleteIfExists(testfile)
	}

	test("date generation [day] [month] works"){
		assert(10.jan == LocalDate.of(LocalDate.now().getYear(), Month.JANUARY, 10))
		assert(10.feb == LocalDate.of(LocalDate.now().getYear(), Month.FEBRUARY, 10))
		assert(10.mar == LocalDate.of(LocalDate.now().getYear(), Month.MARCH, 10))
		assert(10.apr == LocalDate.of(LocalDate.now().getYear(), Month.APRIL, 10))
		assert(10.may == LocalDate.of(LocalDate.now().getYear(), Month.MAY, 10))
		assert(10.jun == LocalDate.of(LocalDate.now().getYear(), Month.JUNE, 10))
		assert(10.jul == LocalDate.of(LocalDate.now().getYear(), Month.JULY, 10))
		assert(10.aug == LocalDate.of(LocalDate.now().getYear(), Month.AUGUST, 10))
		assert(10.sep == LocalDate.of(LocalDate.now().getYear(), Month.SEPTEMBER, 10))
		assert(10.oct == LocalDate.of(LocalDate.now().getYear(), Month.OCTOBER, 10))
		assert(10.dec == LocalDate.of(LocalDate.now().getYear(), Month.DECEMBER, 10))
	}

	test("compound date generation works"){
		assert((31 jan 2016) + 2.days ==  LocalDate.of(2016, 2, 2))
		assert((31 jan 2016) + 2.months ==  LocalDate.of(2016, 3, 31))
		assert((31 jan 2016) + 2.years ==  LocalDate.of(2018, 1, 31))
	}



}