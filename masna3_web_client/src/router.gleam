import gleam/uri.{type Uri}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import modem

// Local
import types
import views/index
import views/not_found
import views/register_file

pub fn update(
  model: types.Model,
  msg: types.Msg,
) -> #(types.Model, Effect(types.Msg)) {
  case msg {
    types.UserNavigatedTo(route:) -> #(
      types.Model(..model, route:),
      effect.none(),
    )
  }
}

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

  let model = types.Model(route:)

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
        types.RegisterFile -> register_file.view()
        types.NotFound(_) -> not_found.view()
      }
    }),
  ])
}
