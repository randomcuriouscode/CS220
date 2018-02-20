import java.nio.file.Paths
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.StandardOpenOption

object PathImplicits {
	implicit class StringPath(s: String){
		def /(other: String):java.nio.file.Path =
		  Paths.get(s).resolve(other)
		def /(other: java.nio.file.Path):java.nio.file.Path =
			Paths.get(s).resolve(other)
	}

	implicit class PathExtras(path: java.nio.file.Path){
		def /(other: String):java.nio.file.Path =
			path.resolve(other)
		def /(other: java.nio.file.Path):java.nio.file.Path = 
			path.resolve(other)

		def write(content: String): Unit = {
			Files.write(path, content.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
			/*val out : PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(path.toFile(), false)))
			out.write(content)
			out.close()*/
		}

		def append(content: String): Unit = {
			Files.write(path, content.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)

		}

		def read(): String = {
			new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
		}
	}
}