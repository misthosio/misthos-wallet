// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Icons = require("./Icons.bs.js");
var React = require("react");
var Colors = require("./Colors.bs.js");
var ViewCommon = require("./ViewCommon.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var MaterialUi_AppBar = require("@jsiebern/bs-material-ui/src/MaterialUi_AppBar.bs.js");
var MaterialUi_Toolbar = require("@jsiebern/bs-material-ui/src/MaterialUi_Toolbar.bs.js");
var MaterialUi_IconButton = require("@jsiebern/bs-material-ui/src/MaterialUi_IconButton.bs.js");

var component = ReasonReact.statelessComponent("Header");

var flex_ = Css.style(/* :: */[
      Css.flex(1),
      /* [] */0
    ]);

var appBar = Css.style(/* :: */[
      Css.backgroundColor(Colors.white),
      /* :: */[
        Css.boxShadow(undefined, undefined, undefined, undefined, undefined, Colors.white),
        /* [] */0
      ]
    ]);

var logo = Css.style(/* :: */[
      Css.hover(/* :: */[
            Css.backgroundColor(Css.transparent),
            /* [] */0
          ]),
      /* :: */[
        Css.borderRadius(Css.px(0)),
        /* [] */0
      ]
    ]);

var Styles = /* module */[
  /* flex_ */flex_,
  /* appBar */appBar,
  /* logo */logo
];

function make(onClickLogo, hrefLogo, onClickMenu, _children) {
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
          /* render */(function (_self) {
              var match;
              var exit = 0;
              if (onClickLogo !== undefined && hrefLogo === undefined) {
                match = /* tuple */[
                  onClickLogo,
                  undefined
                ];
              } else {
                exit = 1;
              }
              if (exit === 1) {
                match = hrefLogo !== undefined ? /* tuple */[
                    undefined,
                    hrefLogo
                  ] : /* tuple */[
                    undefined,
                    undefined
                  ];
              }
              var href = match[1];
              return ReasonReact.element(undefined, undefined, MaterialUi_AppBar.make(appBar, undefined, /* Static */982536398, undefined, undefined, undefined, undefined, undefined, /* array */[ReasonReact.element(undefined, undefined, MaterialUi_Toolbar.make(undefined, undefined, undefined, undefined, undefined, /* array */[
                                        href !== undefined ? React.createElement("a", {
                                                href: href
                                              }, Icons.logoSolid) : ReasonReact.element(undefined, undefined, MaterialUi_IconButton.make(logo, /* Inherit */-72987685, undefined, true, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, match[0], undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[Icons.logoSolid])),
                                        React.createElement("div", {
                                              className: flex_
                                            }),
                                        onClickMenu !== undefined ? ReasonReact.element(undefined, undefined, MaterialUi_IconButton.make(undefined, /* Inherit */-72987685, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, onClickMenu, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[Icons.menu])) : null
                                      ]))]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
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
