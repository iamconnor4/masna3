import lustre/element.{type Element}
import lustre/element/html

import types/msg.{type Msg}
import types/route

pub fn view() -> List(Element(Msg)) {
  [
    html.p([], [html.text("Index page")]),
    html.ul([], [
      html.li([], [
        html.a([route.href(route.RegisterFile)], [html.text("Register File")]),
      ]),
      html.li([], [
        html.a([route.href(route.ConfirmFile)], [html.text("Confirm File")]),
      ]),
      html.li([], [
        html.a([route.href(route.DeleteFile)], [html.text("Delete File")]),
      ]),
    ]),
  ]
}
