import domain/file/confirm_file.{type Model as ConfirmFileModel}
import domain/file/delete_file.{type Model as DeleteFileModel}
import domain/file/register_file.{type Model as RegisterFileModel}
import types/route.{type Route}

pub type Model {
  Model(
    route: Route,
    register_file: RegisterFileModel,
    confirm_file: ConfirmFileModel,
    delete_file: DeleteFileModel,
  )
}
