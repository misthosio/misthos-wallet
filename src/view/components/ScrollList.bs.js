// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Glamor = require("glamor");
var ViewCommon = require("../ViewCommon.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

var customScrollBar = Glamor.css({
      "::-webkit-scrollbar-track": {
        borderLeft: "9px solid white",
        borderRight: "1px solid white",
        backgroundColor: "#000"
      },
      "::-webkit-scrollbar": {
        width: "11px",
        backgroundColor: "#fff"
      },
      "::-webkit-scrollbar-thumb": {
        borderLeft: "8px solid white",
        backgroundColor: "#000"
      }
    });

var scrollContainer = Css.style(/* :: */[
      Css.unsafe("flex", "1 1 auto"),
      /* :: */[
        Css.overflowX(Css.hidden),
        /* :: */[
          Css.overflowY(Css.auto),
          /* :: */[
            Css.minHeight(Css.px(0)),
            /* :: */[
              Css.width(/* `percent */[
                    -119887163,
                    100.0
                  ]),
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

var Styles = /* module */[
  /* customScrollBar */customScrollBar,
  /* scrollContainer */scrollContainer
];

var containerStyles = Css.style(/* :: */[
      Css.height(/* `percent */[
            -119887163,
            100.0
          ]),
      /* :: */[
        Css.display(/* flex */-1010954439),
        /* :: */[
          Css.flexDirection(Css.column),
          /* [] */0
        ]
      ]
    ]);

var component = ReasonReact.statelessComponent("ScrollList");

function make(children) {
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
              return ReasonReact.createDomElement("div", {
                          className: scrollContainer + (" " + customScrollBar)
                        }, children);
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var text = ViewCommon.text;

var extractString = ViewCommon.extractString;

var ignoreEvent = ViewCommon.ignoreEvent;

exports.text = text;
exports.extractString = extractString;
exports.ignoreEvent = ignoreEvent;
exports.Styles = Styles;
exports.containerStyles = containerStyles;
exports.component = component;
exports.make = make;
/* customScrollBar Not a pure module */
