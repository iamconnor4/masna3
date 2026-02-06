import gleam/option.{type Option, None, Some}
import gleam/string

import utils/uuid

pub type ValidationError {
  InvalidUUID(String)
  InvalidMimeType(String)
}

pub fn validate_uuid(input_uuid: String) -> Option(ValidationError) {
  case uuid.from_string(input_uuid) {
    Error(_) -> Some(InvalidUUID("Invalid UUID"))
    Ok(parsed_uuid) -> {
      case uuid.version(parsed_uuid) {
        uuid.V7 -> None
        _ -> Some(InvalidUUID("Must be a V7 UUID"))
      }
    }
  }
}

pub fn validate_mime_type(input_mime_type: String) -> Option(ValidationError) {
  case string.split(input_mime_type, "/") {
    ["", ""] | ["", _] | [_, ""] | [_] | [] | [_, _, _, ..] ->
      Some(InvalidMimeType("Invalid MIME type"))
    _ -> None
  }
}
