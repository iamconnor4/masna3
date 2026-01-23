import lustre/element.{type Element}
import lustre/element/html

import types

pub fn view() -> List(Element(types.Msg)) {
  [html.p([], [html.text("Not found")])]
}
