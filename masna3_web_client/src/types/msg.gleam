import domain/register_file.{type Msg as RegisterFileMsg}
import types/route.{type Route}

pub type Msg {
  UserNavigatedTo(route: Route)
  RegisterFileMsg(RegisterFileMsg)
}
