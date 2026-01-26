import lustre/effect.{type Effect}

import domain/register_file
import types/model.{type Model, Model}
import types/msg.{type Msg, RegisterFileMsg, UserNavigatedTo}

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserNavigatedTo(route:) -> #(Model(..model, route:), effect.none())

    RegisterFileMsg(register_msg) -> {
      let #(new_register_file_model, register_file_effect) =
        register_file.update(model.register_file, register_msg)

      #(
        Model(..model, register_file: new_register_file_model),
        effect.map(register_file_effect, RegisterFileMsg),
      )
    }
  }
}
