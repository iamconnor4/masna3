import lustre/element.{type Element}
import lustre/element/html

import config
import types

pub fn view() -> Element(types.Msg) {
  html.nav([], [
    html.a([types.href(types.Index)], [html.text(config.app_name)]),
  ])
}
