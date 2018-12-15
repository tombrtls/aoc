package tombrtls.adventofcode

import java.io.{File, PrintWriter, Writer}

import scala.io.Source

object FileHelper {
  def readLines(resourcePath: String): Seq[String] = {
    val uri = getClass().getResource(resourcePath).toURI
    using(Source.fromFile(uri, "utf-8")) { source =>
      source.getLines().toList
    }
  }

  private def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  def fileWriter(resourcePath: String, write: (Writer) => Unit): Unit = {
    val file = new File(resourcePath)
    if (file.exists() == false) {
      file.createNewFile()
    } else {
      file.delete()
      file.createNewFile()
    }
    using (new PrintWriter(file)) { writer =>
      write(writer)
    }
  }
}
