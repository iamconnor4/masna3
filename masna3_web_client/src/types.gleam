import gleam/uri
import lustre/attribute.{type Attribute}

pub type Model {
  Model(route: Route)
}

pub type Msg {
  UserNavigatedTo(route: Route)
}

pub type Route {
  Index
  RegisterFile
  NotFound(uri: uri.Uri)
}

pub fn href(route: Route) -> Attribute(msg) {
  let url = case route {
    Index -> "/"
    RegisterFile -> "/register_file"
    NotFound(_) -> "/404"
  }

  attribute.href(url)
}
