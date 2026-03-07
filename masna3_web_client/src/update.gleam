import lustre/effect.{type Effect}

import domain/confirm_file
import domain/delete_file
import domain/register_file
import types/model.{type Model, Model}
import types/msg.{
  type Msg, ConfirmFileMsg, DeleteFileMsg, RegisterFileMsg, UserNavigatedTo,
}

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserNavigatedTo(route:) -> {
      #(Model(..model, route:), effect.none())
    }

    RegisterFileMsg(register_msg) -> {
      let #(new_register_file_model, register_file_effect) =
        register_file.update(model.register_file, register_msg)

      #(
        Model(..model, register_file: new_register_file_model),
        effect.map(register_file_effect, RegisterFileMsg),
      )
    }

    ConfirmFileMsg(confirm_msg) -> {
      let #(new_confirm_file_model, confirm_file_effect) =
        confirm_file.update(model.confirm_file, confirm_msg)

      #(
        Model(..model, confirm_file: new_confirm_file_model),
        effect.map(confirm_file_effect, ConfirmFileMsg),
      )
    }

    DeleteFileMsg(delete_msg) -> {
      let #(new_delete_file_model, delete_file_effect) =
        delete_file.update(model.delete_file, delete_msg)

      #(
        Model(..model, delete_file: new_delete_file_model),
        effect.map(delete_file_effect, DeleteFileMsg),
      )
    }
  }
}
