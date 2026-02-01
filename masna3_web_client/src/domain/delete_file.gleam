import gleam/http/response
import gleam/json
import gleam/option.{type Option, None, Some}

import lustre/effect.{type Effect}
import rsvp

import config
import types/domain.{type FileId, FileId}

pub type Msg {
  UserChangedFileId(String)
  UserSubmittedForm
  ApiReturnedDeletedFile(Result(response.Response(String), rsvp.Error))
}

pub type Model {
  Model(
    file_id: FileId,
    delete_file_response: Option(Result(response.Response(String), rsvp.Error)),
  )
}

pub fn send(file_id: FileId) -> Effect(Msg) {
  let FileId(id) = file_id
  let url = config.api_base_url <> "/files/" <> id <> "/delete"
  let handler = rsvp.expect_ok_response(ApiReturnedDeletedFile)

  rsvp.delete(url, json.object([]), handler)
}

pub fn init() -> Model {
  let file_id = FileId("")

  Model(file_id:, delete_file_response: None)
}

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserChangedFileId(v) -> {
      let file_id = FileId(v)

      #(Model(..model, file_id:), effect.none())
    }
    UserSubmittedForm -> #(model, send(model.file_id))
    ApiReturnedDeletedFile(Ok(deleted_file)) -> #(
      Model(..model, delete_file_response: Some(Ok(deleted_file))),
      effect.none(),
    )
    ApiReturnedDeletedFile(Error(err)) -> #(
      Model(..model, delete_file_response: Some(Error(err))),
      effect.none(),
    )
  }
}
