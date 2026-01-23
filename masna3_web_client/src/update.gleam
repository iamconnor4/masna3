import gleam/option.{Some}

import lustre/effect.{type Effect}

import domain/register_file
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
      effect.map(
        register_file.send(model.register_file_form),
        types.RegisterFileMsg,
      ),
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
