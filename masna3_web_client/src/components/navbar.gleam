import lustre/element.{type Element}
import lustre/element/html

import config
import types/msg.{type Msg}
import types/route

pub fn view() -> Element(Msg) {
  html.nav([], [
    html.a([route.href(route.Index)], [html.text(config.app_name)]),
  ])
}
