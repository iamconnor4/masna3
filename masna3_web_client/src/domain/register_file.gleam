import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}

import lustre/effect.{type Effect}
import rsvp

import config
import validation.{type ValidationError}

pub type Msg {
  UserChangedFileName(String)
  UserChangedMimeType(String)
  UserChangedOwnerId(String)
  UserSubmittedForm
  ApiReturnedRegisteredFile(Result(FileRegistrationResult, rsvp.Error))
}

pub type Model {
  Model(
    register_file_form: FileRegistrationForm,
    validation_errors: List(ValidationError),
    registered_file: Option(Result(FileRegistrationResult, rsvp.Error)),
  )
}

pub type FileRegistrationForm {
  FileRegistrationForm(file_name: String, mime_type: String, owner_id: String)
}

pub type FileRegistrationResult {
  FileRegistrationResult(file_id: String, url: String)
}

pub fn send(form: FileRegistrationForm) -> Effect(Msg) {
  let decoder = {
    use file_id <- decode.field("file_id", decode.string)
    use url <- decode.field("url", decode.string)
    decode.success(FileRegistrationResult(file_id:, url:))
  }

  let body =
    json.object([
      #("file_name", json.string(form.file_name)),
      #("mime_type", json.string(form.mime_type)),
      #("owner_id", json.string(form.owner_id)),
    ])

  let url = config.api_base_url <> "/files/register"
  let handler = rsvp.expect_json(decoder, ApiReturnedRegisteredFile)

  rsvp.post(url, body, handler)
}

pub fn init() -> Model {
  let register_file_form =
    FileRegistrationForm(file_name: "", mime_type: "", owner_id: "")

  Model(register_file_form:, registered_file: None, validation_errors: [])
}

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserChangedFileName(v) -> {
      let form = FileRegistrationForm(..model.register_file_form, file_name: v)

      #(Model(..model, register_file_form: form), effect.none())
    }
    UserChangedMimeType(v) -> {
      let form = FileRegistrationForm(..model.register_file_form, mime_type: v)

      #(Model(..model, register_file_form: form), effect.none())
    }
    UserChangedOwnerId(v) -> {
      let form = FileRegistrationForm(..model.register_file_form, owner_id: v)

      #(Model(..model, register_file_form: form), effect.none())
    }
    UserSubmittedForm -> {
      let validation_errors = validate(model.register_file_form)
      let new_model = Model(..model, validation_errors:, registered_file: None)
      case validation_errors {
        [] -> #(new_model, send(new_model.register_file_form))
        _ -> #(new_model, effect.none())
      }
    }
    ApiReturnedRegisteredFile(Ok(registered_file)) -> #(
      Model(..model, registered_file: Some(Ok(registered_file))),
      effect.none(),
    )
    ApiReturnedRegisteredFile(Error(err)) -> #(
      Model(..model, registered_file: Some(Error(err))),
      effect.none(),
    )
  }
}

fn validate(form: FileRegistrationForm) -> List(ValidationError) {
  [
    validation.validate_uuid(form.owner_id),
    validation.validate_mime_type(form.mime_type),
  ]
  |> option.values()
}
