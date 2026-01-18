import lustre/element.{type Element}
import lustre/element/html
import types

pub fn view() -> List(Element(types.Msg)) {
  [
    html.p([], [html.text("Index page")]),
    html.ul([], [
      html.li([], [
        html.a([types.href(types.RegisterFile)], [html.text("Register File")]),
      ]),
    ]),
  ]
}
