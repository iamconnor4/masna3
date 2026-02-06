import gleam/list

import lustre/element.{type Element}
import lustre/element/html

import types/msg.{type Msg}
import validation.{type ValidationError, InvalidMimeType, InvalidUUID}

pub fn view(err: List(ValidationError)) -> Element(Msg) {
  html.div([], [
    html.h3([], [html.text("Invalid Input")]),
    html.ul(
      [],
      err
        |> list.map(fn(e) {
          case e {
            InvalidUUID(s) -> html.li([], [html.text(s)])
            InvalidMimeType(s) -> html.li([], [html.text(s)])
          }
        }),
    ),
  ])
}
