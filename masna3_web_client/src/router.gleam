import gleam/option.{None}
import gleam/uri.{type Uri}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import modem

// Local
//import api/register_file as api_register_file
import types
import views/index
import views/not_found
import views/register_file

fn parse_route(uri: Uri) -> types.Route {
  case uri.path_segments(uri.path) {
    [] | [""] -> types.Index
    ["register_file"] -> types.RegisterFile
    _ -> types.NotFound(uri:)
  }
}

pub fn init(_) -> #(types.Model, Effect(types.Msg)) {
  let route = case modem.initial_uri() {
    Ok(uri) -> parse_route(uri)
    Error(_) -> types.Index
  }

  let register_file_form =
    types.FileRegistrationForm(file_name: "", mime_type: "", owner_id: "")

  let model = types.Model(route:, register_file_form:, registered_file: None)

  let effect =
    modem.init(fn(uri) {
      uri
      |> parse_route
      |> types.UserNavigatedTo
    })

  #(model, effect)
}

pub fn view(model: types.Model) -> Element(types.Msg) {
  html.div([], [
    html.nav([], [
      html.a([types.href(types.Index)], [html.text("Masna3 Web Client")]),
    ]),
    html.main([], {
      case model.route {
        types.Index -> index.view()
        types.RegisterFile -> register_file.view(model)
        types.NotFound(_) -> not_found.view()
      }
    }),
  ])
}
