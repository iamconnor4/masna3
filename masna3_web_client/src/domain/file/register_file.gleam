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
  UserChangedProcessId(String)
  UserSubmittedForm
  ApiReturnedRegisteredFile(Result(FileRegistrationResult, rsvp.Error))
}

pub type Model {
  Model(
    register_file_form: FileRegistrationForm,
    validation_errors: List(ValidationError),
    register_file_response: Option(Result(FileRegistrationResult, rsvp.Error)),
  )
}

pub type FileRegistrationForm {
  FileRegistrationForm(
    file_name: String,
    mime_type: String,
    owner_id: String,
    process_id: Option(String),
  )
}

pub type FileRegistrationResult {
  FileRegistrationResult(
    file_id: String,
    url: String,
    process_id: Option(String),
  )
}

pub fn send(form: FileRegistrationForm) -> Effect(Msg) {
  let decoder = {
    use file_id <- decode.field("file_id", decode.string)
    use url <- decode.field("url", decode.string)
    use process_id <- decode.optional_field(
      "process_id",
      None,
      decode.map(decode.string, Some),
    )
    decode.success(FileRegistrationResult(file_id:, url:, process_id:))
  }

  let process_id = case form.process_id {
    None -> []
    Some(id) -> [#("process_id", json.string(id))]
  }

  let body =
    json.object([
      #("file_name", json.string(form.file_name)),
      #("mime_type", json.string(form.mime_type)),
      #("owner_id", json.string(form.owner_id)),
      ..process_id
    ])

  let url = config.api_base_url <> "/files/register"
  let handler = rsvp.expect_json(decoder, ApiReturnedRegisteredFile)

  rsvp.post(url, body, handler)
}

pub fn init() -> Model {
  let register_file_form =
    FileRegistrationForm(
      file_name: "",
      mime_type: "",
      owner_id: "",
      process_id: None,
    )

  Model(
    register_file_form:,
    register_file_response: None,
    validation_errors: [],
  )
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
    UserChangedProcessId(v) -> {
      let process_id = case v {
        "" -> None
        _ -> Some(v)
      }

      let form = FileRegistrationForm(..model.register_file_form, process_id:)

      #(Model(..model, register_file_form: form), effect.none())
    }
    UserSubmittedForm -> {
      let validation_errors = validate(model.register_file_form)
      let new_model =
        Model(..model, validation_errors:, register_file_response: None)
      case validation_errors {
        [] -> #(new_model, send(new_model.register_file_form))
        _ -> #(new_model, effect.none())
      }
    }
    ApiReturnedRegisteredFile(Ok(registered_file)) -> #(
      Model(..model, register_file_response: Some(Ok(registered_file))),
      effect.none(),
    )
    ApiReturnedRegisteredFile(Error(err)) -> #(
      Model(..model, register_file_response: Some(Error(err))),
      effect.none(),
    )
  }
}

fn validate(form: FileRegistrationForm) -> List(ValidationError) {
  [
    validation.validate_uuid(form.owner_id),
    validation.validate_mime_type(form.mime_type),
    option.map(form.process_id, validation.validate_uuid) |> option.flatten,
  ]
  |> option.values()
}
