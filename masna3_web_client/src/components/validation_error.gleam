import gleam/list

import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

import types/msg.{type Msg}
import validation.{type ValidationError, InvalidMimeType, InvalidUUID}

pub fn view(err: List(ValidationError)) -> Element(Msg) {
  html.div(
    [
      attribute.class(
        "w-82 mb-2 p-3 border border-yellow-400 rounded-md bg-yellow-100 text-center",
      ),
    ],
    [
      html.h3([attribute.class("underline")], [
        html.text("Invalid Input"),
      ]),
      html.ul(
        [attribute.class("list-disc list-inside")],
        err
          |> list.map(fn(e) {
            case e {
              InvalidUUID(s) -> html.li([], [html.text(s)])
              InvalidMimeType(s) -> html.li([], [html.text(s)])
            }
          }),
      ),
    ],
  )
}
