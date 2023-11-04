package codecs

import cats.Show
import codecs.JsonReader.JsonReaderOps

import scala.annotation.tailrec
import scala.language.implicitConversions

sealed trait Json
object Json {
  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonDouble(value: Double) extends Json
  final case class JsonArray(values: List[Json]) extends Json
  final case class JsonObject(values: Map[String, Json]) extends Json

  implicit val show: Show[Json] = Show.show {
    case JsonNull          => "null"
    case JsonString(value) => s""""$value""""
    case JsonInt(value)    => value.toString
    case JsonDouble(value) => value.toString
    case JsonArray(items) =>
      items.map(show.show).mkString("[\n  ", ",\n  ", "\n]")
    case JsonObject(entries) =>
      entries
        .map { case (key, value) => s""""$key": ${show.show(value)}""" }
        .mkString("{\n  ", ",\n  ", "\n}")
  }

  implicit def listStringToJsonArray(strings: List[String]): JsonArray = {
    JsonArray(strings.map(JsonString))
  }

}
