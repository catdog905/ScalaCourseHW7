package codecs
import codecs.Json._
import codecs.JsonReader._
import codecs.JsonWriter._

trait Person {
  def name: String
  def age: Int
}
object Person {
  private def parseFieldFromMap[T](values: Map[String, Json], fieldName: String)(implicit
    valueReader: JsonReader[T]
  ): Either[ReaderError, T] =
    values.get(fieldName) match {
      case Some(value) => value.as[T]
      case None        => Left(AbsentField(fieldName))
    }

  case class University(name: String, city: String, country: String, qsRank: Int)
  implicit val universityJsonWriter: JsonWriter[University] = { case University(name, city, country, qsRank) =>
    JsonObject(
      Map("name" -> name.toJson, "city" -> city.toJson, "country" -> country.toJson, "qsRank" -> qsRank.toJson)
    )
  }
  implicit val universityJsonReader: JsonReader[University] = {
    case JsonObject(values) =>
      (
        parseFieldFromMap(values, "name")(stringJsonReader),
        parseFieldFromMap(values, "city")(stringJsonReader),
        parseFieldFromMap(values, "country")(stringJsonReader),
        parseFieldFromMap(values, "qsRank")(intJsonReader)
      ) match {
        case (Right(name), Right(city), Right(country), Right(qsRank)) =>
          Right(University(name, city, country, qsRank))
        case (nameEither, cityEither, countryEither, qsRankEither) =>
          Left(
            ReaderErrors(
              List(nameEither, cityEither, countryEither, qsRankEither).collect({ case Left(error) => error }),
              ""
            )
          )
      }
    case _ => Left(WrongType("university"))
  }

  case class Student(name: String, age: Int, university: University) extends Person

  implicit def personJsonWriter[A <: Person]: JsonWriter[A] = person =>
    JsonObject(Map("name" -> person.name.toJson, "age" -> person.age.toJson))

  implicit val studentJsonWriter: JsonWriter[Student] = { case Student(name, age, university) =>
    JsonObject(
      Map("name" -> name.toJson, "age" -> age.toJson, "university" -> university.toJson)
    )
  }
  implicit val studentJsonReader: JsonReader[Student] = {
    case JsonObject(values) =>
      (
        parseFieldFromMap(values, "name")(stringJsonReader),
        parseFieldFromMap(values, "age")(intJsonReader),
        parseFieldFromMap(values, "university")(universityJsonReader)
      ) match {
        case (Right(name), Right(age), Right(university)) =>
          Right(Student(name, age, university))
        case (nameEither, ageEither, universityEither) =>
          Left(
            ReaderErrors(
              List(nameEither, ageEither, universityEither).collect({ case Left(error) => error }),
              ""
            )
          )
      }
    case _ => Left(WrongType("student"))
  }

  trait Worker extends Person {
    def salary: Double
  }
  case class Employee(name: String, age: Int, salary: Double) extends Worker
  case class Manager(name: String, age: Int, salary: Double, employees: List[Employee], boss: Option[Manager])
    extends Worker

  implicit def workerJsonWriter[A <: Worker]: JsonWriter[A] = worker =>
    JsonObject(Map("name" -> worker.name.toJson, "age" -> worker.age.toJson, "salary" -> worker.salary.toJson))

  implicit val employeeJsonWriter: JsonWriter[Employee] = { case Employee(name, age, salary) =>
    JsonObject(
      Map(
        "name" -> name.toJson,
        "age" -> age.toJson,
        "salary" -> salary.toJson
      )
    )
  }
  implicit val employeeJsonReader: JsonReader[Employee] = {
    case JsonObject(values) =>
      (
        parseFieldFromMap(values, "name")(stringJsonReader),
        parseFieldFromMap(values, "age")(intJsonReader),
        parseFieldFromMap(values, "salary")(doubleJsonReader)
      ) match {
        case (Right(name), Right(age), Right(salary)) =>
          Right(Employee(name, age, salary))
        case (nameEither, ageEither, salaryEither) =>
          Left(
            ReaderErrors(
              List(nameEither, ageEither, salaryEither).collect({ case Left(error) => error }),
              ""
            )
          )
      }
    case _ => Left(WrongType("employee"))
  }

  implicit def managerJsonWriter(implicit managerJsonWriter: JsonWriter[Manager]): JsonWriter[Manager] = {
    case Manager(name, age, salary, employees, boss) =>
      JsonObject(
        Map(
          "name" -> name.toJson,
          "age" -> age.toJson,
          "salary" -> salary.toJson,
          "employees" -> employees.toJson,
          "boss" -> boss.toJson
        )
      )
  }

  implicit lazy val managerJsonReader: JsonReader[Manager] = {
    case JsonObject(values) =>
      (
        parseFieldFromMap(values, "name")(stringJsonReader),
        parseFieldFromMap(values, "age")(intJsonReader),
        parseFieldFromMap(values, "salary")(doubleJsonReader),
        parseFieldFromMap(values, "employees")(listJsonReader(employeeJsonReader)),
        parseFieldFromMap(values, "boss")(managerJsonReader)
      ) match {
        case (Right(name), Right(age), Right(salary), Right(employees), Right(boss)) =>
          Right(Manager(name, age, salary, employees, Some(boss)))
        case (Right(name), Right(age), Right(salary), Right(employees), Left(AbsentField(_, _))) =>
          Right(Manager(name, age, salary, employees, None))
        case (nameEither, ageEither, salaryEither, employeesEither, bossEither) =>
          Left(
            ReaderErrors(
              List(nameEither, ageEither, salaryEither, employeesEither, bossEither).collect({ case Left(error) =>
                error
              }),
              ""
            )
          )
      }
    case _ => Left(WrongType("manager"))
  }
}
