import components/rsvp_error
import components/validation_error
import domain/register_file.{
  FileRegistrationResult, UserChangedFileName, UserChangedMimeType,
  UserChangedOwnerId, UserSubmittedForm,
}
import gleam/list
import gleam/option.{None, Some}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import types/model.{type Model}
import types/msg.{type Msg, RegisterFileMsg}

pub fn view(model: Model) -> List(Element(Msg)) {
  [
    html.div([attribute.class("w-full flex items-center flex-col")], [
      html.h2([attribute.class("mb-4")], [html.text("Register File")]),
      case list.is_empty(model.register_file.validation_errors) {
        True -> element.none()
        False -> validation_error.view(model.register_file.validation_errors)
      },
      case model.register_file.register_file_response {
        Some(Ok(FileRegistrationResult(file_id, url))) ->
          html.div([], [
            html.h3([], [html.text("File registered")]),
            html.p([], [html.text("File ID: " <> file_id)]),
            html.p([], [html.text("URL: " <> url)]),
          ])
        Some(Error(err)) -> rsvp_error.view(err, "File not registered")
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
              html.text("File Name"),
            ]),
            html.input([
              attribute.id("file_name"),
              attribute.class(
                "p-2 border border-neutral-300 rounded-md focus:outline-none focus:border-neutral-400 text-sm",
              ),
              event.on_input(fn(value) {
                RegisterFileMsg(UserChangedFileName(value))
              }),
            ]),
          ]),
          html.div([attribute.class("flex flex-col gap-1")], [
            html.label([attribute.class("text-sm text-neutral-600")], [
              html.text("Mime Type"),
            ]),
            html.input([
              attribute.id("mime_type"),
              attribute.class(
                "p-2 border border-neutral-300 rounded-md focus:outline-none focus:border-neutral-400 text-sm",
              ),
              event.on_input(fn(value) {
                RegisterFileMsg(UserChangedMimeType(value))
              }),
            ]),
          ]),
          html.div([attribute.class("flex flex-col gap-1")], [
            html.label([attribute.class("text-sm text-neutral-600")], [
              html.text("Owner ID"),
            ]),
            html.input([
              attribute.id("owner_id"),
              attribute.class(
                "p-2 border border-neutral-300 rounded-md focus:outline-none focus:border-neutral-400 text-sm",
              ),
              event.on_input(fn(value) {
                RegisterFileMsg(UserChangedOwnerId(value))
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
            [html.text("Register File")],
          ),
        ],
      ),
    ]),
  ]
}

fn handle_submit(_) -> Msg {
  RegisterFileMsg(UserSubmittedForm)
}
