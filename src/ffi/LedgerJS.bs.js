// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");

function error(param) {
  if (typeof param === "number") {
    return "Unknown";
  } else if (param.tag) {
    return "U2F_5(" + (param[0] + ")");
  } else {
    return "U2FNotSupported(" + (param[0] + ")");
  }
}

function decodeError(error) {
  var match = error.id;
  switch (match) {
    case "U2FNotSupported" : 
        return /* U2FNotSupported */Block.__(0, [error.message]);
    case "U2F_5" : 
        return /* U2F_5 */Block.__(1, [error.message]);
    default:
      return /* Unknown */0;
  }
}

exports.error = error;
exports.decodeError = decodeError;
/* No side effect */
