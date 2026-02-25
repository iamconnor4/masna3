import components/rsvp_error
import components/validation_error
import domain/delete_file.{UserChangedFileId, UserSubmittedForm}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import types/model.{type Model}
import types/msg.{type Msg, DeleteFileMsg}

pub fn view(model: Model) -> List(Element(Msg)) {
  [
    html.div([attribute.class("w-full flex items-center flex-col")], [
      html.h2([attribute.class("mb-4")], [html.text("Delete File")]),
      case list.is_empty(model.delete_file.validation_errors) {
        True -> element.none()
        False -> validation_error.view(model.delete_file.validation_errors)
      },
      case model.delete_file.delete_file_response {
        Some(Ok(s)) ->
          html.div([], [
            html.h3([], [html.text("File deleted")]),
            html.p([], [html.text("Status: " <> int.to_string(s.status))]),
            html.p([], [html.text("Body: " <> s.body)]),
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
      html.form(
        [
          event.on_submit(handle_submit),
          attribute.class("flex flex-col gap-4 w-82"),
        ],
        [
          html.div([attribute.class("flex flex-col gap-1")], [
            html.label([attribute.class("text-sm text-neutral-600")], [
              html.text("File ID"),
            ]),
            html.input([
              attribute.id("file_id"),
              attribute.class(
                "p-2 border border-neutral-300 rounded-md focus:outline-none focus:border-neutral-400 text-sm",
              ),
              event.on_input(fn(value) {
                DeleteFileMsg(UserChangedFileId(value))
              }),
            ]),
          ]),
          html.button(
            [
              attribute.type_("submit"),
              attribute.class(
                "px-4 py-3 rounded-md border border-neutral-300 hover:border-neutral-400 hover:bg-neutral-100 text-sm font-medium text-neutral-800 cursor-pointer",
              ),
            ],
            [html.text("Delete File")],
          ),
        ],
      ),
    ]),
  ]
}

fn handle_submit(_) -> Msg {
  DeleteFileMsg(UserSubmittedForm)
}
