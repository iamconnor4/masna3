import gleam/dynamic/decode
import gleam/json

import lustre/effect.{type Effect}
import rsvp

import config
import types

pub fn send(form: types.FileRegistrationForm) -> Effect(types.RegisterFileMsg) {
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

  let url = config.api_base_url <> "/files/register"
  let handler = rsvp.expect_json(decoder, types.ApiReturnedRegisteredFile)

  rsvp.post(url, body, handler)
}
