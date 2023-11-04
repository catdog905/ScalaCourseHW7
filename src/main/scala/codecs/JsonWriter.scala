package codecs
import Json._

trait JsonWriter[A] {
  def write(a: A): Json
}

object JsonWriter {
  // Summoner function
  def apply[A](implicit writer: JsonWriter[A]): JsonWriter[A] = writer

  implicit class JsonWriterOps[A](val a: A) extends AnyVal {
    def toJson(implicit writer: JsonWriter[A]): Json = writer.write(a)
  }

  implicit val stringJsonWriter: JsonWriter[String] = value => JsonString(value)
  implicit val intJsonWriter: JsonWriter[Int] = value => JsonInt(value)
  implicit val doubleJsonWriter: JsonWriter[Double] = value => JsonDouble(value)
  implicit def listJsonWriter[T](implicit writer: JsonWriter[T]): JsonWriter[List[T]] =
    list => JsonArray(list.map(_.toJson))
  implicit def optionJsonWriter[T](implicit writer: JsonWriter[T]): JsonWriter[Option[T]] = {
    case Some(value) => value.toJson
    case None        => JsonNull
  }
  implicit val noneJsonWriter: JsonWriter[None.type] = _ => JsonNull
}
