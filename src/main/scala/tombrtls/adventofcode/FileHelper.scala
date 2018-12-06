package tombrtls.adventofcode

import scala.io.Source

object FileHelper {
  def readLines(resourcePath: String): Seq[String] = {
    val uri = getClass().getResource(resourcePath).toURI
    using(Source.fromFile(uri, "utf-8")) { source =>
      source.getLines().toList
    }
  }

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}
