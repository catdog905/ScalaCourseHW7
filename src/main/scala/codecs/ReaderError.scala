package codecs

sealed abstract class ReaderError
case class WrongType(field: String, message: String = "Wrong field type") extends ReaderError
case class AbsentField(field: String, message: String = "Absent field") extends ReaderError
case class ReaderErrors(list: List[ReaderError], field: String, message: String = "Several errors")
  extends ReaderError
