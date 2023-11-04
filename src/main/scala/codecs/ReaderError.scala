package codecs

sealed abstract class ReaderError(message: String, field: String)
case class WrongType(field: String, message: String = "Wrong field type") extends ReaderError(message, field)
case class AbsentField(field: String, message: String = "Absent field") extends ReaderError(message, field)
case class ReaderErrors(list: List[ReaderError], field: String, message: String = "Several errors")
  extends ReaderError(message, field)
