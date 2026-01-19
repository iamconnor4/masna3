import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/option.{Some}
import lustre/effect.{type Effect}
import rsvp
import types

pub fn update(
  model: types.Model,
  msg: types.Msg,
) -> #(types.Model, Effect(types.Msg)) {
  case msg {
    types.UserNavigatedTo(route:) -> #(
      types.Model(..model, route:),
      effect.none(),
    )

    types.RegisterFileMsg(register_msg) ->
      handle_register_file(model, register_msg)
  }
}

fn handle_register_file(
  model: types.Model,
  msg: types.RegisterFileMsg,
) -> #(types.Model, Effect(types.Msg)) {
  case msg {
    types.UserChangedFileName(v) -> {
      let form =
        types.FileRegistrationForm(..model.register_file_form, file_name: v)

      #(types.Model(..model, register_file_form: form), effect.none())
    }
    types.UserChangedMimeType(v) -> {
      let form =
        types.FileRegistrationForm(..model.register_file_form, mime_type: v)

      #(types.Model(..model, register_file_form: form), effect.none())
    }
    types.UserChangedOwnerId(v) -> {
      let form =
        types.FileRegistrationForm(..model.register_file_form, owner_id: v)

      #(types.Model(..model, register_file_form: form), effect.none())
    }
    types.UserSubmittedFileForm -> #(
      model,
      effect.map(register_file(model.register_file_form), types.RegisterFileMsg),
    )

    types.ApiReturnedRegisteredFile(Ok(registered_file)) -> #(
      types.Model(..model, registered_file: Some(Ok(registered_file))),
      effect.none(),
    )

    types.ApiReturnedRegisteredFile(Error(err)) -> #(
      types.Model(..model, registered_file: Some(Error(err))),
      effect.none(),
    )
  }
}

fn register_file(
  form: types.FileRegistrationForm,
) -> Effect(types.RegisterFileMsg) {
  let decoder = {
    use file_id <- decode.field("file_id", decode.string)
    use url <- decode.field("url", decode.string)
    decode.success(types.FileRegistrationResult(file_id:, url:))
  }

  let body =
    json.object([
      #("file_name", json.string(form.file_name)),
      #("mime_type", json.string(form.mime_type)),
      #("owner_id", json.string(form.owner_id)),
    ])

  // For 
  io.println(json.to_string(body))

  let url = "http://localhost:8085/api/files/register"
  let handler = rsvp.expect_json(decoder, types.ApiReturnedRegisteredFile)

  rsvp.post(url, body, handler)
}
