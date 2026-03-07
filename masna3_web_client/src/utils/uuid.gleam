//// UUID Parsing Utilities
//// 
//// This code is extracted from the youid library (https://hex.pm/packages/youid)
//// for browser compatibility. The full library depends on gleam_crypto, which is
//// currently incompatible for Javascript in the browser.

import gleam/string

pub opaque type Uuid {
  Uuid(value: BitArray)
}

pub type Version {
  V1
  V2
  V3
  V4
  V5
  V7
  VUnknown
}

pub fn from_string(in: String) -> Result(Uuid, Nil) {
  let hex = case in {
    "urn:uuid:" <> in -> in
    _ -> in
  }

  case to_bit_array_helper(hex) {
    Ok(bits) -> Ok(Uuid(value: bits))
    Error(_) -> Error(Nil)
  }
}

fn to_bit_array_helper(str: String) -> Result(BitArray, Nil) {
  to_bitstring_help(str, 0, <<>>)
}

fn to_bitstring_help(
  str: String,
  index: Int,
  acc: BitArray,
) -> Result(BitArray, Nil) {
  case string.pop_grapheme(str) {
    Error(Nil) if index == 32 -> Ok(acc)
    Ok(#("-", rest)) if index < 32 -> to_bitstring_help(rest, index, acc)
    Ok(#(c, rest)) if index < 32 ->
      case hex_to_int(c) {
        Ok(i) -> to_bitstring_help(rest, index + 1, <<acc:bits, i:size(4)>>)
        Error(_) -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

// Hex Helpers
fn hex_to_int(c: String) -> Result(Int, Nil) {
  let i = case c {
    "0" -> 0
    "1" -> 1
    "2" -> 2
    "3" -> 3
    "4" -> 4
    "5" -> 5
    "6" -> 6
    "7" -> 7
    "8" -> 8
    "9" -> 9
    "a" | "A" -> 10
    "b" | "B" -> 11
    "c" | "C" -> 12
    "d" | "D" -> 13
    "e" | "E" -> 14
    "f" | "F" -> 15
    _ -> 16
  }
  case i {
    16 -> Error(Nil)
    x -> Ok(x)
  }
}

pub fn version(uuid: Uuid) -> Version {
  let assert <<_:48, ver:4, _:76>> = uuid.value
  decode_version(ver)
}

fn decode_version(int: Int) -> Version {
  case int {
    1 -> V1
    2 -> V2
    3 -> V3
    4 -> V4
    5 -> V5
    7 -> V7
    _ -> VUnknown
  }
}
