// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Theme = require("../Theme.bs.js");
var React = require("react");
var Colors = require("../Colors.bs.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var ViewCommon = require("../ViewCommon.bs.js");
var BreakPoints = require("../BreakPoints.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var WarningBanner = require("./WarningBanner.bs.js");
var MaterialUi_Tab = require("@jsiebern/bs-material-ui/src/MaterialUi_Tab.bs.js");
var MaterialUi_Tabs = require("@jsiebern/bs-material-ui/src/MaterialUi_Tabs.bs.js");

var component = ReasonReact.reducerComponent("Grid");

var gapSM = String(Theme.space(4)) + "px 0px";

var gapXS = String(Theme.space(2)) + "px 0px";

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
  var tmp$1;
  switch (variant) {
    case 0 : 
        tmp$1 = "\n              \". title1 .\"\n              \". area3 .\"\n              " + (
          warning$1 ? "\". warning .\"" : ""
        );
        break;
    case 1 : 
        tmp$1 = "\n               \". title1 .\"\n               \". area3 . \"\n               \". area4 .\"\n               " + (
          warning$1 ? "\" . warning  .\"" : ""
        );
        break;
    case 2 : 
        tmp$1 = "\n               \". title1 .\"\n               \". area3 . \"\n               \". area4 .\"\n               \". area5 .\"\n               " + (
          warning$1 ? "\" . warning .\"" : ""
        );
        break;
    case 3 : 
        tmp$1 = (
          warning$1 ? "\". warning  .\"" : ""
        ) + "\n                \". area1 .\"\n                \". area2 .\"\n                \". tabs .\"\n                \". area3 . \"\n                \". area4 .\"\n                ";
        break;
    
  }
  var tmp$2;
  switch (variant) {
    case 0 : 
        tmp$2 = "[tBegin] min-content [tEnd] min-content" + (
          warning$1 ? " [wBegin] min-content [wEnd]" : ""
        );
        break;
    case 1 : 
    case 2 : 
        tmp$2 = "[tBegin] min-content [tEnd] min-content min-content" + (
          warning$1 ? " [wBegin] min-content [wEnd]" : ""
        );
        break;
    case 3 : 
        tmp$2 = (
          warning$1 ? "[wBegin] min-content [wEnd] " : ""
        ) + "min-content min-content [tBegin] min-content [tEnd] min-content min-content ";
        break;
    
  }
  return Css.style(/* :: */[
              Css.display(Css.grid),
              /* :: */[
                BreakPoints.sm(/* :: */[
                      Css.unsafe("gridGap", gapSM),
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
                            /* [] */0
                          ]
                        ]
                      ]
                    ]),
                /* :: */[
                  BreakPoints.xs(/* :: */[
                        Css.unsafe("gridGap", gapXS),
                        /* :: */[
                          Css.unsafe("gridTemplateAreas", tmp$1),
                          /* :: */[
                            Css.unsafe("gridTemplateColumns", "[begin] 16px minmax(100px, 9fr) 16px [end]"),
                            /* :: */[
                              Css.unsafe("gridTemplateRows", tmp$2),
                              /* [] */0
                            ]
                          ]
                        ]
                      ]),
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

function mobileHidden(hidden) {
  return Css.style(/* :: */[
              BreakPoints.sm(/* :: */[
                    Css.display(Css.block),
                    /* [] */0
                  ]),
              /* :: */[
                BreakPoints.xs(/* :: */[
                      Css.display(hidden ? Css.none : Css.block),
                      /* [] */0
                    ]),
                /* [] */0
              ]
            ]);
}

function title(variant) {
  var match = variant === /* V4 */3;
  return Css.style(/* :: */[
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
                          /* :: */[
                            BreakPoints.sm(/* :: */[
                                  Css.display(Css.inline),
                                  /* [] */0
                                ]),
                            /* :: */[
                              BreakPoints.xs(/* :: */[
                                    Css.display(match ? Css.none : Css.inline),
                                    /* [] */0
                                  ]),
                              /* [] */0
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var tabs = Css.style(/* :: */[
      Css.unsafe("gridColumn", "begin / end"),
      /* :: */[
        Css.unsafe("gridRow", "tBegin / tEnd"),
        /* :: */[
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
                      BreakPoints.sm(/* :: */[
                            Css.display(Css.none),
                            /* [] */0
                          ]),
                      /* [] */0
                    ]
                  ]
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
              BreakPoints.sm(/* :: */[
                    Css.unsafe("borderImageSlice", "1"),
                    /* :: */[
                      Css.unsafe("borderImageSource", Colors.uGradient),
                      /* :: */[
                        Css.unsafe("borderWidth", "0px 0px 4px 0px"),
                        /* [] */0
                      ]
                    ]
                  ]),
              /* [] */0
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
  /* gapSM */gapSM,
  /* gapXS */gapXS,
  /* grid */grid,
  /* area */area,
  /* mobileHidden */mobileHidden,
  /* title */title,
  /* tabs */tabs,
  /* titleBg */titleBg,
  /* warningBg */warningBg
];

function make(title1, title2, area1, area2, area3, area4, area5, warning, _children) {
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
          /* render */(function (param) {
              var send = param[/* send */3];
              var state = param[/* state */1];
              var tabs$1 = ReasonReact.element(undefined, undefined, MaterialUi_Tabs.make(undefined, undefined, tabs, undefined, true, undefined, (function (param, i) {
                          return Curry._1(send, /* ActiveTab */[i]);
                        }), true, undefined, undefined, undefined, undefined, undefined, state[/* activeTab */0], undefined, undefined, /* array */[
                        ReasonReact.element(undefined, undefined, MaterialUi_Tab.make(undefined, undefined, undefined, undefined, undefined, Js_primitive.some(Js_option.getWithDefault(null, title1)), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[])),
                        ReasonReact.element(undefined, undefined, MaterialUi_Tab.make(undefined, undefined, undefined, undefined, undefined, Js_primitive.some(Js_option.getWithDefault(null, title2)), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[]))
                      ]));
              var variant = area1 !== undefined || area3 === undefined ? /* V4 */3 : (
                  area4 !== undefined ? (
                      area5 !== undefined ? /* V3 */2 : /* V2 */1
                    ) : (
                      area5 !== undefined ? /* V4 */3 : /* V1 */0
                    )
                );
              var match = variant === /* V4 */3;
              return React.createElement("div", {
                          className: grid(variant, warning)
                        }, warning !== undefined ? React.createElement("div", {
                                key: "warningBg",
                                className: WarningBanner.Styles[/* warningBg */1] + (" " + warningBg)
                              }) : null, React.createElement("div", {
                              key: "titleBg",
                              className: titleBg
                            }), Belt_Array.concatMany(/* array */[
                              Belt_Array.map(/* array */[
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
                                      title(variant)
                                    ],
                                    /* tuple */[
                                      title2,
                                      "title2",
                                      title(variant)
                                    ],
                                    /* tuple */[
                                      area3,
                                      "area3",
                                      mobileHidden(state[/* activeTab */0] !== 0 && variant === /* V4 */3)
                                    ],
                                    /* tuple */[
                                      area4,
                                      "area4",
                                      mobileHidden(state[/* activeTab */0] !== 1 && variant === /* V4 */3)
                                    ],
                                    /* tuple */[
                                      area5,
                                      "area5",
                                      ""
                                    ]
                                  ], (function (param) {
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
                                    })),
                              /* array */[match ? tabs$1 : null]
                            ]));
            }),
          /* initialState */(function (param) {
              return /* record */[/* activeTab */0];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, param) {
              return /* Update */Block.__(0, [/* record */[/* activeTab */action[0]]]);
            }),
          /* jsElementWrapped */component[/* jsElementWrapped */13]
        ];
}

var text = ViewCommon.text;

var extractString = ViewCommon.extractString;

var ignoreEvent = ViewCommon.ignoreEvent;

exports.text = text;
exports.extractString = extractString;
exports.ignoreEvent = ignoreEvent;
exports.component = component;
exports.Styles = Styles;
exports.make = make;
/* component Not a pure module */
