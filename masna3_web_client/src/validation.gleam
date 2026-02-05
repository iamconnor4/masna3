import gleam/option.{type Option, None, Some}

import utils/uuid

pub type ValidationError {
  InvalidUUID(String)
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
