import gleam/int
import gleam/list
import gleam/option.{None, Some}

import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

import domain/delete_file.{UserChangedFileId, UserSubmittedForm}
import types/model.{type Model}
import types/msg.{type Msg, DeleteFileMsg}

import components/rsvp_error
import components/validation_error

pub fn view(model: Model) -> List(Element(Msg)) {
  [
    html.div([], [
      html.h2([], [html.text("Delete File")]),

      case list.is_empty(model.delete_file.validation_errors) {
        True -> element.none()
        False -> validation_error.view(model.delete_file.validation_errors)
      },

      case model.delete_file.delete_file_response {
        Some(Ok(s)) ->
          html.div([], [
            html.h3([], [html.text("File deleted")]),
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
        Some(Error(err)) -> rsvp_error.view(err, "File not deleted")
        None -> element.none()
      },

      html.form([event.on_submit(handle_submit)], [
        html.div([], [
          html.label([], [html.text("File Id")]),
          html.input([
            attribute.id("file_id"),
            event.on_input(fn(value) { DeleteFileMsg(UserChangedFileId(value)) }),
          ]),
        ]),

        html.button([attribute.type_("submit")], [html.text("Delete File")]),
      ]),
    ]),
  ]
}

fn handle_submit(_) -> Msg {
  DeleteFileMsg(UserSubmittedForm)
}
