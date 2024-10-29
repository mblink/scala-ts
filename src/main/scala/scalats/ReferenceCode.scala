package scalats

case class ReferenceCode[ValueType[_]](
  valueType: ValueType[Generated],
  codecType: Generated,
  codecValue: Generated,
)
