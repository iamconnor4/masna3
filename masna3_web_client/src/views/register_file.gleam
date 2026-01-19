import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import rsvp
import types

pub fn view(model: types.Model) -> List(Element(types.Msg)) {
  [
    html.div([], [
      html.h2([], [html.text("Register File")]),

      case model.registered_file {
        Some(Ok(types.FileRegistrationResult(file_id, url))) ->
          html.div([], [
            html.h3([], [html.text("File registered")]),
            html.p([], [html.text("File ID: " <> file_id)]),
            html.p([], [html.text("URL: " <> url)]),
          ])
        Some(Error(err)) ->
          html.div([], [
            html.h3([], [html.text("File not registered")]),
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
        None -> html.div([], [])
      },

      html.form([event.on_submit(handle_submit)], [
        html.div([], [
          html.label([], [html.text("File Name")]),
          html.input([
            attribute.id("file_name"),
            event.on_input(fn(value) {
              types.RegisterFileMsg(types.FileNameChanged(value))
            }),
          ]),
        ]),
        html.div([], [
          html.label([], [html.text("Mime Type")]),
          html.input([
            attribute.id("mime_type"),
            event.on_input(fn(value) {
              types.RegisterFileMsg(types.MimeTypeChanged(value))
            }),
          ]),
        ]),
        html.div([], [
          html.label([], [html.text("Owner ID")]),
          html.input([
            attribute.id("owner_id"),
            event.on_input(fn(value) {
              types.RegisterFileMsg(types.OwnerIdChanged(value))
            }),
          ]),
        ]),

        html.button([attribute.type_("submit")], [html.text("Register File")]),
      ]),
    ]),
  ]
}

fn handle_submit(_) -> types.Msg {
  types.RegisterFileMsg(types.Submitted)
}
