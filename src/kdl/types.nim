import std/[options, tables]
import bigints

type
  KdlError* = object of CatchableError
  KdlLexerError* = object of KdlError
  KdlParserError* = object of KdlError

  KValKind* = enum
    KEmpty,
    KString,
    KFloat,
    KBool,
    KNull,
    KInt,
    KInt8, KInt16, KInt32, KInt64,
    KUInt8, KUInt16, KUInt32, KUInt64,
    KBigInt,  # For integers that don't fit in int64/uint64
    KFloat32, KFloat64,
    KDate, KTime, KDateTime, KDuration

  KdlVal* = object
    tag*: Option[string] # Type annotation

    case kind*: KValKind
    of KString:
      str*: string
    of KFloat:
      fnum*: float
    of KBool:
      boolean*: bool
    of KNull, KEmpty:
      discard
    of KInt:
      num*: int64
    of KInt8:
      i8*: int8
    of KInt16:
      i16*: int16
    of KInt32:
      i32*: int32
    of KInt64:
      i64*: int64
    of KUInt8:
      u8*: uint8
    of KUInt16:
      u16*: uint16
    of KUInt32:
      u32*: uint32
    of KUInt64:
      u64*: uint64
    of KBigInt:
      bigint*: BigInt
    of KFloat32:
      f32*: float32
    of KFloat64:
      f64*: float64
    of KDate:
      date*: string # Using string for now, could be Nim's Time.Date
    of KTime:
      time*: string # Using string for now, could be Nim's Time.Time
    of KDateTime:
      datetime*: string # Using string for now, could be Nim's Time.DateTime
    of KDuration:
      duration*: string # Using string for now, could be Nim's Time.Duration

  KdlProp* = tuple[key: string, val: KdlVal]

  KdlNode* = object
    tag*: Option[string]
    name*: string
    args*: seq[KdlVal]
    props*: Table[string, KdlVal]
    children*: seq[KdlNode]

  KdlDoc* = seq[KdlNode]

  KdlPrefs*[T] = object
    path*: string
    default*: T
    content*: T
