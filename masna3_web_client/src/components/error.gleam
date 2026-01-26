import gleam/int
import gleam/list
import gleam/string

import lustre/element.{type Element}
import lustre/element/html
import rsvp

import types/msg.{type Msg}

pub fn view(err: rsvp.Error, context: String) -> Element(Msg) {
  html.div([], [
    html.h3([], [html.text(context)]),
    case err {
      rsvp.NetworkError -> html.p([], [html.text("NetworkError")])
      rsvp.BadBody -> html.p([], [html.text("BadBody")])
      rsvp.BadUrl(s) -> html.p([], [html.text("BadUrl: " <> s)])
      rsvp.HttpError(s) ->
        html.div([], [
          html.text("HttpError"),
          html.p([], [html.text("Status:" <> int.to_string(s.status))]),
          html.p([], [html.text("Body:" <> s.body)]),
          html.p([], [html.text("Headers:")]),
          html.ul(
            [],
            s.headers
              |> list.map(fn(pair) {
                let #(header, value) = pair
                html.li([], [html.text(header <> " : " <> value)])
              }),
          ),
        ])
      rsvp.JsonError(s) ->
        html.div([], [
          html.p([], [html.text("JsonError")]),
          html.p([], [html.text("DecodeErr: " <> string.inspect(s))]),
        ])
      rsvp.UnhandledResponse(s) ->
        html.div([], [
          html.p([], [html.text("UnhandledResponse")]),
          html.p([], [html.text("Status:" <> int.to_string(s.status))]),
          html.p([], [html.text("Body:" <> s.body)]),
          html.p([], [html.text("Headers:")]),
          html.ul(
            [],
            s.headers
              |> list.map(fn(entry) {
                let #(header, value) = entry
                html.li([], [html.text(header <> " : " <> value)])
              }),
          ),
        ])
    },
  ])
}
