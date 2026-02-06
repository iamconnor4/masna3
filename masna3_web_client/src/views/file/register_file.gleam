import gleam/list
import gleam/option.{None, Some}

import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

import domain/register_file.{
  FileRegistrationResult, UserChangedFileName, UserChangedMimeType,
  UserChangedOwnerId, UserSubmittedForm,
}
import types/model.{type Model}
import types/msg.{type Msg, RegisterFileMsg}

import components/rsvp_error
import components/validation_error

pub fn view(model: Model) -> List(Element(Msg)) {
  [
    html.div([], [
      html.h2([], [html.text("Register File")]),

      case list.is_empty(model.register_file.validation_errors) {
        True -> element.none()
        False -> validation_error.view(model.register_file.validation_errors)
      },

      case model.register_file.registered_file {
        Some(Ok(FileRegistrationResult(file_id, url))) ->
          html.div([], [
            html.h3([], [html.text("File registered")]),
            html.p([], [html.text("File ID: " <> file_id)]),
            html.p([], [html.text("URL: " <> url)]),
          ])
        Some(Error(err)) -> rsvp_error.view(err, "File not registered")
        None -> element.none()
      },

      html.form([event.on_submit(handle_submit)], [
        html.div([], [
          html.label([], [html.text("File Name")]),
          html.input([
            attribute.id("file_name"),
            event.on_input(fn(value) {
              RegisterFileMsg(UserChangedFileName(value))
            }),
          ]),
        ]),
        html.div([], [
          html.label([], [html.text("Mime Type")]),
          html.input([
            attribute.id("mime_type"),
            event.on_input(fn(value) {
              RegisterFileMsg(UserChangedMimeType(value))
            }),
          ]),
        ]),
        html.div([], [
          html.label([], [html.text("Owner ID")]),
          html.input([
            attribute.id("owner_id"),
            event.on_input(fn(value) {
              RegisterFileMsg(UserChangedOwnerId(value))
            }),
          ]),
        ]),

        html.button([attribute.type_("submit")], [html.text("Register File")]),
      ]),
    ]),
  ]
}

fn handle_submit(_) -> Msg {
  RegisterFileMsg(UserSubmittedForm)
}
