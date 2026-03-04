package scalats

case class ReferenceCode[Value[_]](
  codecType: Generated,
  codecInstance: Generated,
  valueType: Value[Generated],
  valueInstance: Value[Any => Generated],
)
