import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import types/msg.{type Msg}
import types/route

pub fn view() -> List(Element(Msg)) {
  [
    html.div(
      [
        attribute.class("flex flex-col justify-center items-center"),
      ],
      [
        html.h2([attribute.class("mb-2")], [html.text("Options")]),
        html.div([attribute.class("flex flex-col gap-2")], [
          html.a(
            [
              route.href(route.RegisterFile),
              attribute.class(
                "px-4 py-3 rounded-md border border-neutral-300 hover:border-neutral-400 hover:bg-neutral-100",
              ),
            ],
            [
              html.p(
                [
                  attribute.class(
                    "text-center text-sm font-medium text-neutral-800",
                  ),
                ],
                [
                  html.text("Register File"),
                ],
              ),
            ],
          ),
          html.a(
            [
              route.href(route.ConfirmFile),
              attribute.class(
                "px-4 py-3 rounded-md border border-neutral-300 hover:border-neutral-400 hover:bg-neutral-100",
              ),
            ],
            [
              html.p(
                [
                  attribute.class(
                    "text-center text-sm font-medium text-neutral-800",
                  ),
                ],
                [
                  html.text("Confirm File"),
                ],
              ),
            ],
          ),
          html.a(
            [
              route.href(route.DeleteFile),
              attribute.class(
                "px-4 py-3 rounded-md border border-neutral-300 hover:border-neutral-400 hover:bg-neutral-100",
              ),
            ],
            [
              html.p(
                [
                  attribute.class(
                    "text-center text-sm font-medium text-neutral-800",
                  ),
                ],
                [
                  html.text("Delete File"),
                ],
              ),
            ],
          ),
        ]),
      ],
    ),
  ]
}
