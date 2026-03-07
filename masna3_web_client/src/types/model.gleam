import domain/confirm_file.{type Model as ConfirmFileModel}
import domain/delete_file.{type Model as DeleteFileModel}
import domain/register_file.{type Model as RegisterFileModel}
import types/route.{type Route}

pub type Model {
  Model(
    route: Route,
    register_file: RegisterFileModel,
    confirm_file: ConfirmFileModel,
    delete_file: DeleteFileModel,
  )
}
