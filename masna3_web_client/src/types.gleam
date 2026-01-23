import gleam/option.{type Option}
import gleam/uri

import lustre/attribute.{type Attribute}
import rsvp

pub type Model {
  Model(
    route: Route,
    register_file_form: FileRegistrationForm,
    registered_file: Option(Result(FileRegistrationResult, rsvp.Error)),
  )
}

pub type Route {
  Index
  RegisterFile
  NotFound(uri: uri.Uri)
}

/// Utility function for type-safe routing.
/// Assuming this function is kept in sync
/// with `parse_route` from `router.gleam`
pub fn href(route: Route) -> Attribute(msg) {
  let url = case route {
    Index -> "/"
    RegisterFile -> "/register_file"
    NotFound(_) -> "/404"
  }

  attribute.href(url)
}

// Messages
pub type Msg {
  UserNavigatedTo(route: Route)
  RegisterFileMsg(RegisterFileMsg)
}

pub type RegisterFileMsg {
  UserChangedFileName(String)
  UserChangedMimeType(String)
  UserChangedOwnerId(String)
  UserSubmittedFileForm
  ApiReturnedRegisteredFile(Result(FileRegistrationResult, rsvp.Error))
}

pub type FileRegistrationForm {
  FileRegistrationForm(file_name: String, mime_type: String, owner_id: String)
}

pub type FileRegistrationResult {
  FileRegistrationResult(file_id: String, url: String)
}
