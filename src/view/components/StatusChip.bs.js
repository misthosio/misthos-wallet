// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");
var Theme = require("../Theme.bs.js");
var Colors = require("../Colors.bs.js");
var ViewCommon = require("../ViewCommon.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var MaterialUi_Chip = require("@jsiebern/bs-material-ui/src/MaterialUi_Chip.bs.js");

var component = ReasonReact.statelessComponent("StatusChip");

function chip(status) {
  var tmp;
  switch (status) {
    case 0 : 
        tmp = Colors.grayedOut;
        break;
    case 1 : 
        tmp = Css.rgba(245, 166, 35, 0.2);
        break;
    case 2 : 
        tmp = Css.rgba(255, 50, 83, 0.2);
        break;
    case 3 : 
        tmp = Css.rgba(2, 162, 180, 0.2);
        break;
    
  }
  var tmp$1;
  switch (status) {
    case 0 : 
        tmp$1 = Css.hex("7f7f7f");
        break;
    case 1 : 
        tmp$1 = Colors.warning;
        break;
    case 2 : 
        tmp$1 = Colors.error;
        break;
    case 3 : 
        tmp$1 = Colors.misthosTeal;
        break;
    
  }
  return Css.style(/* :: */[
              Css.backgroundColor(tmp),
              /* :: */[
                Css.color(tmp$1),
                /* :: */[
                  Css.fontFamily(Theme.sourceSansPro),
                  /* :: */[
                    Css.fontWeight(400),
                    /* :: */[
                      Css.fontSize(Css.px(12)),
                      /* :: */[
                        Css.textTransform(Css.uppercase),
                        /* :: */[
                          Css.borderRadius(Css.px(0)),
                          /* :: */[
                            Css.minWidth(Css.px(Theme.space(11))),
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var Styles = /* module */[/* chip */chip];

function make(status, label, _) {
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
              var label$1 = ViewCommon.text(label);
              return ReasonReact.element(undefined, undefined, MaterialUi_Chip.make(undefined, chip(status), undefined, undefined, undefined, Js_primitive.some(label$1), undefined, undefined, undefined, undefined, undefined, undefined, /* array */[]));
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
