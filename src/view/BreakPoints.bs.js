// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Css = require("bs-css/src/Css.js");

function xs(styles) {
  return Css.media("(min-width: 0px)", styles);
}

function sm(styles) {
  return Css.media("(min-width: 600px)", styles);
}

function md(styles) {
  return Css.media("(min-width: 960px)", styles);
}

function lg(styles) {
  return Css.media("(min-width: 1280px)", styles);
}

function xl(styles) {
  return Css.media("(min-width: 1920px)", styles);
}

exports.xs = xs;
exports.sm = sm;
exports.md = md;
exports.lg = lg;
exports.xl = xl;
/* Css Not a pure module */
