import domain/register_file.{type Model as RegisterFileModel}
import types/route.{type Route}

pub type Model {
  Model(route: Route, register_file: RegisterFileModel)
}
