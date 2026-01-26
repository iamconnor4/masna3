import gleam/uri.{type Uri}

import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import modem

import domain/confirm_file
import domain/register_file
import types/model.{type Model, Model}
import types/msg.{type Msg, UserNavigatedTo}
import types/route.{type Route}

import components/navbar
import views/file/confirm_file as confirm_file_view
import views/file/register_file as register_file_view
import views/index
import views/not_found

fn parse_route(uri: Uri) -> Route {
  case uri.path_segments(uri.path) {
    [] | [""] -> route.Index
    ["register_file"] -> route.RegisterFile
    ["confirm_file"] -> route.ConfirmFile
    _ -> route.NotFound(uri:)
  }
}

pub fn init(_) -> #(Model, Effect(Msg)) {
  let route = case modem.initial_uri() {
    Ok(uri) -> parse_route(uri)
    Error(_) -> route.Index
  }

  let model =
    Model(
      route:,
      register_file: register_file.init(),
      confirm_file: confirm_file.init(),
    )

  let effect =
    modem.init(fn(uri) {
      uri
      |> parse_route
      |> UserNavigatedTo
    })

  #(model, effect)
}

pub fn view(model: Model) -> Element(Msg) {
  html.div([], [
    navbar.view(),
    html.main([], {
      case model.route {
        route.Index -> index.view()
        route.RegisterFile -> register_file_view.view(model)
        route.ConfirmFile -> confirm_file_view.view(model)
        route.NotFound(_) -> not_found.view()
      }
    }),
  ])
}
