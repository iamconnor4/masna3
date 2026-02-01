import gleam/uri

import lustre/attribute.{type Attribute}

pub type Route {
  Index
  RegisterFile
  ConfirmFile
  DeleteFile
  NotFound(uri: uri.Uri)
}

/// Utility function for type-safe routing.
/// Assuming this function is kept in sync
/// with `parse_route` from `router.gleam`
pub fn href(route: Route) -> Attribute(msg) {
  let url = case route {
    Index -> "/"
    RegisterFile -> "/register_file"
    ConfirmFile -> "/confirm_file"
    DeleteFile -> "/delete_file"
    NotFound(_) -> "/404"
  }

  attribute.href(url)
}
