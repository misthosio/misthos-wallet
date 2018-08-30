// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Theme = require("../Theme.bs.js");
var React = require("react");
var Colors = require("../Colors.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var WarningBanner = require("./WarningBanner.bs.js");

var component = ReasonReact.statelessComponent("Grid");

var gap = String(Theme.space(4)) + "px 0px";

function grid(variant, warning) {
  var match = warning === undefined;
  var warning$1 = match ? false : true;
  var tmp;
  switch (variant) {
    case 0 : 
        tmp = "\n              \". title1 .\"\n              \". area3 .\"\n              " + (
          warning$1 ? "\". warning .\"" : ""
        );
        break;
    case 1 : 
        tmp = "\n              \". title1 . title2 .\"\n              \". area3 . area4 .\"\n           " + (
          warning$1 ? "\" . warning warning warning .\"" : ""
        );
        break;
    case 2 : 
        tmp = "\n           \". title1 . title2 .\"\n           \". area3 . area4 .\"\n           \". area5 area5 area5 .\"\n           " + (
          warning$1 ? "\" . warning warning warning .\"" : ""
        );
        break;
    case 3 : 
        tmp = (
          warning$1 ? "\" . warning warning warning .\"" : ""
        ) + "\n              \". area1 . area2 .\"\n              \". title1 . title2 .\"\n              \". area3 . area4 .\"\n              ";
        break;
    
  }
  return Css.style(/* :: */[
              Css.display(Css.grid),
              /* :: */[
                Css.unsafe("gridGap", gap),
                /* :: */[
                  Css.unsafe("gridTemplateAreas", tmp),
                  /* :: */[
                    Css.unsafe("gridTemplateColumns", variant !== 0 ? "[begin] minmax(24px, 1fr) minmax(368px, 4fr) minmax(24px, 1fr) minmax(368px, 4fr) minmax(24px, 1fr) [end]" : "[begin] minmax(24px, 1fr) minmax(368px, 9fr) minmax(24px, 1fr) [end]"),
                    /* :: */[
                      Css.unsafe("gridTemplateRows", variant !== 2 ? (
                              variant >= 3 ? (
                                  warning$1 ? "[wBegin] min-content [wEnd] " : ""
                                ) + "min-content [tBegin] min-content [tEnd] auto" : "[tBegin] min-content [tEnd] auto" + (
                                  warning$1 ? " [wBegin] min-content [wEnd]" : ""
                                )
                            ) : "[tBegin] min-content [tEnd] auto min-content" + (
                              warning$1 ? " [wBegin] min-content [wEnd]" : ""
                            )),
                      /* :: */[
                        Css.width(/* `percent */[
                              -119887163,
                              100.0
                            ]),
                        /* :: */[
                          Css.height(/* `percent */[
                                -119887163,
                                100.0
                              ]),
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

function area(area$1) {
  return Css.style(/* :: */[
              Css.unsafe("gridArea", area$1),
              /* :: */[
                Css.minHeight(Css.px(0)),
                /* [] */0
              ]
            ]);
}

var title = Css.style(/* :: */[
      Css.fontFamily(Theme.oswald),
      /* :: */[
        Css.height(Css.px(45)),
        /* :: */[
          Css.fontSize(Css.px(30)),
          /* :: */[
            Css.fontWeight(600),
            /* :: */[
              Css.color(Colors.white),
              /* :: */[
                Css.textTransform(Css.uppercase),
                /* :: */[
                  Css.marginBottom(Css.px(4)),
                  /* [] */0
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

var titleBg = Css.style(/* :: */[
      Css.unsafe("gridColumn", "begin / end"),
      /* :: */[
        Css.unsafe("gridRow", "tBegin / tEnd"),
        /* :: */[
          Css.backgroundColor(Colors.black),
          /* :: */[
            Css.borderBottomStyle(Css.solid),
            /* :: */[
              Css.unsafe("borderImageSlice", "1"),
              /* :: */[
                Css.unsafe("borderImageSource", Colors.uGradient),
                /* :: */[
                  Css.unsafe("borderWidth", "0px 0px 4px 0px"),
                  /* [] */0
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

var warningBg = Css.style(/* :: */[
      Css.unsafe("gridColumn", "begin / end"),
      /* :: */[
        Css.unsafe("gridRow", "wBegin / wEnd"),
        /* [] */0
      ]
    ]);

var Styles = /* module */[
  /* gap */gap,
  /* grid */grid,
  /* area */area,
  /* title */title,
  /* titleBg */titleBg,
  /* warningBg */warningBg
];

function make(title1, title2, area1, area2, area3, area4, area5, warning, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function () {
              var variant = area1 !== undefined || area3 === undefined ? /* V4 */3 : (
                  area4 !== undefined ? (
                      area5 !== undefined ? /* V3 */2 : /* V2 */1
                    ) : (
                      area5 !== undefined ? /* V4 */3 : /* V1 */0
                    )
                );
              return React.createElement("div", {
                          className: grid(variant, warning)
                        }, warning !== undefined ? React.createElement("div", {
                                key: "warningBg",
                                className: WarningBanner.Styles[/* warningBg */1] + (" " + warningBg)
                              }) : null, React.createElement("div", {
                              key: "titleBg",
                              className: titleBg
                            }), $$Array.map((function (param) {
                                var item = param[0];
                                if (item !== undefined) {
                                  var area$1 = param[1];
                                  return React.createElement("div", {
                                              key: area$1,
                                              className: area(area$1) + (" " + param[2])
                                            }, Js_primitive.valFromOption(item));
                                } else {
                                  return null;
                                }
                              }), /* array */[
                              /* tuple */[
                                warning,
                                "warning",
                                WarningBanner.Styles[/* warning */0](false)
                              ],
                              /* tuple */[
                                area1,
                                "area1",
                                ""
                              ],
                              /* tuple */[
                                area2,
                                "area2",
                                ""
                              ],
                              /* tuple */[
                                title1,
                                "title1",
                                title
                              ],
                              /* tuple */[
                                title2,
                                "title2",
                                title
                              ],
                              /* tuple */[
                                area3,
                                "area3",
                                ""
                              ],
                              /* tuple */[
                                area4,
                                "area4",
                                ""
                              ],
                              /* tuple */[
                                area5,
                                "area5",
                                ""
                              ]
                            ]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* jsElementWrapped */component[/* jsElementWrapped */13]
        ];
}

exports.component = component;
exports.Styles = Styles;
exports.make = make;
/* component Not a pure module */
