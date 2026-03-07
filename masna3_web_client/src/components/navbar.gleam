import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

import config
import types/msg.{type Msg}
import types/route

pub fn view() -> Element(Msg) {
  html.nav([attribute.class("border-b border-neutral-300 py-2 px-4 mb-2")], [
    html.a([route.href(route.Index), attribute.class("font-medium")], [
      html.text(config.app_name),
    ]),
  ])
}
