import lustre/element.{type Element}
import lustre/element/html

import types/msg.{type Msg}

pub fn view() -> List(Element(Msg)) {
  [html.p([], [html.text("Not found")])]
}
