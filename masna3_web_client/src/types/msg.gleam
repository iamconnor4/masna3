import domain/confirm_file.{type Msg as ConfirmFileMsg}
import domain/delete_file.{type Msg as DeleteFileMsg}
import domain/register_file.{type Msg as RegisterFileMsg}
import types/route.{type Route}

pub type Msg {
  UserNavigatedTo(route: Route)
  RegisterFileMsg(RegisterFileMsg)
  ConfirmFileMsg(ConfirmFileMsg)
  DeleteFileMsg(DeleteFileMsg)
}
