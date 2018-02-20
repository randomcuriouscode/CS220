import java.time.LocalDate
import java.time.Month
import java.time.Period
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalAmount

object DateImplicits {
	implicit class DateGenerator(value: Int) {
		def days() : TemporalAmount = Period.ofDays(value)
		def months() : TemporalAmount = Period.ofMonths(value)
		def years() : TemporalAmount = Period.ofYears(value)

		def jan() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.JANUARY, value)
		def feb() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.FEBRUARY, value)
		def mar() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.MARCH, value)
		def apr() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.APRIL, value)
		def may() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.MAY, value)
		def jun() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.JUNE, value)
		def jul() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.JULY, value)
		def aug() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.AUGUST, value)
		def sep() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.SEPTEMBER, value)
		def oct() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.OCTOBER, value)
		def nov() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.NOVEMBER, value)
		def dec() : LocalDate = 
			LocalDate.of(LocalDate.now().getYear(), Month.DECEMBER, value)

		def jan(year: Int) : LocalDate = 
			LocalDate.of(year, Month.JANUARY, value)
		def feb(year: Int) : LocalDate = 
			LocalDate.of(year, Month.FEBRUARY, value)
		def mar(year: Int) : LocalDate = 
			LocalDate.of(year, Month.MARCH, value)
		def apr(year: Int) : LocalDate = 
			LocalDate.of(year, Month.APRIL, value)
		def may(year: Int) : LocalDate = 
			LocalDate.of(year, Month.MAY, value)
		def jun(year: Int) : LocalDate = 
			LocalDate.of(year, Month.JUNE, value)
		def jul(year: Int) : LocalDate = 
			LocalDate.of(year, Month.JULY, value)
		def aug(year: Int) : LocalDate = 
			LocalDate.of(year, Month.AUGUST, value)
		def sep(year: Int) : LocalDate = 
			LocalDate.of(year, Month.SEPTEMBER, value)
		def oct(year: Int) : LocalDate = 
			LocalDate.of(year, Month.OCTOBER, value)
		def nov(year: Int) : LocalDate = 
			LocalDate.of(year, Month.NOVEMBER, value)
		def dec(year: Int) : LocalDate = 
			LocalDate.of(year, Month.DECEMBER, value)
	}

	implicit class LocalDateExtension(date: LocalDate) {
		def +(amount: TemporalAmount) = date.plus(amount)
	}
}