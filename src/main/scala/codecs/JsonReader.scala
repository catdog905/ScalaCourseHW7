package codecs

import codecs.Json._

trait JsonReader[A] {
  def read(json: Json): Either[ReaderError, A]
}

object JsonReader {
  // Summoner function
  def apply[A](implicit reader: JsonReader[A]): JsonReader[A] = reader

  implicit class JsonReaderOps(val json: Json) extends AnyVal {
    def as[A](implicit reader: JsonReader[A]): Either[ReaderError, A] = reader.read(json)
  }

  implicit val stringJsonReader: JsonReader[String] = {
    case JsonString(value) => Right(value)
    case _                 => Left(WrongType("string"))
  }
  implicit val intJsonReader: JsonReader[Int] = {
    case JsonInt(value) => Right(value)
    case _              => Left(WrongType(""))
  }
  implicit val doubleJsonReader: JsonReader[Double] = {
    case JsonDouble(value) => Right(value)
    case _                 => Left(WrongType(""))
  }

  private def foldEitherList[T](list: List[Either[ReaderError, T]]): Either[ReaderError, List[T]] = {
    val errors = list.collect({ case Left(error) => error })
    if (errors.isEmpty)
      Right(list.collect({ case Right(value) => value }))
    else
      Left(ReaderErrors(errors, ""))
  }

  implicit def listJsonReader[T](implicit writer: JsonReader[T]): JsonReader[List[T]] = {
    case JsonArray(values) => foldEitherList(values.map(_.as[T]))
    case _                 => Left(WrongType(""))
  }

  implicit def noneJsonReader: JsonReader[None.type] = {
    case JsonNull => Right(None)
    case _        => Left(WrongType(""))
  }

  implicit def optionJsonReader[T](implicit valueReader: JsonReader[T]): JsonReader[Option[T]] = {
    case JsonNull => Right(None)
    case some =>
      valueReader.read(some) match {
        case Left(error)  => Left(error)
        case Right(value) => Right(Some(value))
      }
  }
}
