// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Policy = require("../Policy.bs.js");
var Belt_Set = require("bs-platform/lib/js/belt_Set.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var Json_encode = require("@glennsl/bs-json/src/Json_encode.bs.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var PrimitiveTypes = require("../PrimitiveTypes.bs.js");

function makeProposal(name) {
  return (function (Data) {
      var make = function ($staropt$star, $staropt$star$1, eligibleWhenProposing, proposerId, policy, data) {
        var dependsOnProposals = $staropt$star !== undefined ? Js_primitive.valFromOption($staropt$star) : PrimitiveTypes.ProcessId[/* emptySet */9];
        var dependsOnCompletions = $staropt$star$1 !== undefined ? Js_primitive.valFromOption($staropt$star$1) : PrimitiveTypes.ProcessId[/* emptySet */9];
        return /* record */[
                /* processId */PrimitiveTypes.ProcessId[/* make */10](/* () */0),
                /* dependsOnProposals */dependsOnProposals,
                /* dependsOnCompletions */dependsOnCompletions,
                /* eligibleWhenProposing */eligibleWhenProposing,
                /* proposerId */proposerId,
                /* policy */policy,
                /* data */data
              ];
      };
      var encode = function ($$event) {
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      name
                    ],
                    /* :: */[
                      /* tuple */[
                        "processId",
                        PrimitiveTypes.ProcessId[/* encode */2]($$event[/* processId */0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "dependsOnProposals",
                          Json_encode.array(PrimitiveTypes.ProcessId[/* encode */2], Belt_Set.toArray($$event[/* dependsOnProposals */1]))
                        ],
                        /* :: */[
                          /* tuple */[
                            "dependsOnCompletions",
                            Json_encode.array(PrimitiveTypes.ProcessId[/* encode */2], Belt_Set.toArray($$event[/* dependsOnCompletions */2]))
                          ],
                          /* :: */[
                            /* tuple */[
                              "eligibleWhenProposing",
                              Json_encode.array(PrimitiveTypes.UserId[/* encode */2], Belt_Set.toArray($$event[/* eligibleWhenProposing */3]))
                            ],
                            /* :: */[
                              /* tuple */[
                                "proposerId",
                                PrimitiveTypes.UserId[/* encode */2]($$event[/* proposerId */4])
                              ],
                              /* :: */[
                                /* tuple */[
                                  "policy",
                                  Policy.encode($$event[/* policy */5])
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "data",
                                    Curry._1(Data[/* encode */0], $$event[/* data */6])
                                  ],
                                  /* [] */0
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]);
      };
      var decode = function (raw) {
        var partial_arg = PrimitiveTypes.ProcessId[/* decode */3];
        var partial_arg$1 = PrimitiveTypes.ProcessId[/* decode */3];
        var partial_arg$2 = PrimitiveTypes.UserId[/* decode */3];
        return /* record */[
                /* processId */Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw),
                /* dependsOnProposals */Belt_Set.mergeMany(PrimitiveTypes.ProcessId[/* emptySet */9], Json_decode.field("dependsOnProposals", (function (param) {
                            return Json_decode.array(partial_arg, param);
                          }), raw)),
                /* dependsOnCompletions */Belt_Set.mergeMany(PrimitiveTypes.ProcessId[/* emptySet */9], Json_decode.field("dependsOnCompletions", (function (param) {
                            return Json_decode.array(partial_arg$1, param);
                          }), raw)),
                /* eligibleWhenProposing */Belt_Set.mergeMany(PrimitiveTypes.UserId[/* emptySet */9], Json_decode.field("eligibleWhenProposing", (function (param) {
                            return Json_decode.array(partial_arg$2, param);
                          }), raw)),
                /* proposerId */Json_decode.field("proposerId", PrimitiveTypes.UserId[/* decode */3], raw),
                /* policy */Json_decode.field("policy", Policy.decode, raw),
                /* data */Json_decode.field("data", Data[/* decode */1], raw)
              ];
      };
      return /* module */[
              /* make */make,
              /* encode */encode,
              /* decode */decode
            ];
    });
}

function makeRejection(name) {
  var make = function (processId, rejectorId) {
    return /* record */[
            /* processId */processId,
            /* rejectorId */rejectorId
          ];
  };
  var encode = function ($$event) {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  name
                ],
                /* :: */[
                  /* tuple */[
                    "processId",
                    PrimitiveTypes.ProcessId[/* encode */2]($$event[/* processId */0])
                  ],
                  /* :: */[
                    /* tuple */[
                      "rejectorId",
                      PrimitiveTypes.UserId[/* encode */2]($$event[/* rejectorId */1])
                    ],
                    /* [] */0
                  ]
                ]
              ]);
  };
  var decode = function (raw) {
    return /* record */[
            /* processId */Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw),
            /* rejectorId */Json_decode.field("rejectorId", PrimitiveTypes.UserId[/* decode */3], raw)
          ];
  };
  return /* module */[
          /* make */make,
          /* encode */encode,
          /* decode */decode
        ];
}

function makeEndorsement(name) {
  var make = function (processId, supporterId) {
    return /* record */[
            /* processId */processId,
            /* supporterId */supporterId
          ];
  };
  var encode = function ($$event) {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  name
                ],
                /* :: */[
                  /* tuple */[
                    "processId",
                    PrimitiveTypes.ProcessId[/* encode */2]($$event[/* processId */0])
                  ],
                  /* :: */[
                    /* tuple */[
                      "supporterId",
                      PrimitiveTypes.UserId[/* encode */2]($$event[/* supporterId */1])
                    ],
                    /* [] */0
                  ]
                ]
              ]);
  };
  var decode = function (raw) {
    return /* record */[
            /* processId */Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw),
            /* supporterId */Json_decode.field("supporterId", PrimitiveTypes.UserId[/* decode */3], raw)
          ];
  };
  return /* module */[
          /* make */make,
          /* encode */encode,
          /* decode */decode
        ];
}

function makeAcceptance(name) {
  return (function (Data) {
      var fromProposal = function (param) {
        return /* record */[
                /* processId */param[/* processId */0],
                /* dependsOnCompletions */Belt_Set.union(param[/* dependsOnCompletions */2], param[/* dependsOnProposals */1]),
                /* data */param[/* data */6]
              ];
      };
      var encode = function ($$event) {
        return Json_encode.object_(/* :: */[
                    /* tuple */[
                      "type",
                      name
                    ],
                    /* :: */[
                      /* tuple */[
                        "processId",
                        PrimitiveTypes.ProcessId[/* encode */2]($$event[/* processId */0])
                      ],
                      /* :: */[
                        /* tuple */[
                          "dependsOnCompletions",
                          Json_encode.array(PrimitiveTypes.ProcessId[/* encode */2], Belt_Set.toArray($$event[/* dependsOnCompletions */1]))
                        ],
                        /* :: */[
                          /* tuple */[
                            "data",
                            Curry._1(Data[/* encode */0], $$event[/* data */2])
                          ],
                          /* [] */0
                        ]
                      ]
                    ]
                  ]);
      };
      var decode = function (raw) {
        var partial_arg = PrimitiveTypes.ProcessId[/* decode */3];
        return /* record */[
                /* processId */Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw),
                /* dependsOnCompletions */Belt_Set.mergeMany(PrimitiveTypes.ProcessId[/* emptySet */9], Json_decode.field("dependsOnCompletions", (function (param) {
                            return Json_decode.array(partial_arg, param);
                          }), raw)),
                /* data */Json_decode.field("data", Data[/* decode */1], raw)
              ];
      };
      return /* module */[
              /* fromProposal */fromProposal,
              /* encode */encode,
              /* decode */decode
            ];
    });
}

function makeDenial(name) {
  var fromProposal = function (param) {
    return /* record */[/* processId */param[/* processId */0]];
  };
  var encode = function ($$event) {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  name
                ],
                /* :: */[
                  /* tuple */[
                    "processId",
                    PrimitiveTypes.ProcessId[/* encode */2]($$event[/* processId */0])
                  ],
                  /* [] */0
                ]
              ]);
  };
  var decode = function (raw) {
    return /* record */[/* processId */Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw)];
  };
  return /* module */[
          /* fromProposal */fromProposal,
          /* encode */encode,
          /* decode */decode
        ];
}

function makeAbort(name) {
  var fromProposal = function (param) {
    return /* record */[/* processId */param[/* processId */0]];
  };
  var encode = function ($$event) {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  name
                ],
                /* :: */[
                  /* tuple */[
                    "processId",
                    PrimitiveTypes.ProcessId[/* encode */2]($$event[/* processId */0])
                  ],
                  /* [] */0
                ]
              ]);
  };
  var decode = function (raw) {
    return /* record */[/* processId */Json_decode.field("processId", PrimitiveTypes.ProcessId[/* decode */3], raw)];
  };
  return /* module */[
          /* fromProposal */fromProposal,
          /* encode */encode,
          /* decode */decode
        ];
}

function makeProcess(name) {
  return (function (funarg) {
      var processName = name + "ApprovalProcess";
      var Proposed = makeProposal(name + "Proposed")(funarg);
      var Rejected = makeRejection(name + "Rejected");
      var Endorsed = makeEndorsement(name + "Endorsed");
      var Accepted = makeAcceptance(name + "Accepted")(funarg);
      var Denied = makeDenial(name + "Denied");
      var Aborted = makeAbort(name + "Aborted");
      var dataEq = function (dataA, dataB) {
        return Caml_obj.caml_equal(Curry._1(funarg[/* encode */0], dataA), Curry._1(funarg[/* encode */0], dataB));
      };
      return /* module */[
              /* processName */processName,
              /* dataEq */dataEq,
              /* Proposed */Proposed,
              /* Rejected */Rejected,
              /* Endorsed */Endorsed,
              /* Accepted */Accepted,
              /* Denied */Denied,
              /* Aborted */Aborted
            ];
    });
}

exports.makeProposal = makeProposal;
exports.makeRejection = makeRejection;
exports.makeEndorsement = makeEndorsement;
exports.makeAcceptance = makeAcceptance;
exports.makeDenial = makeDenial;
exports.makeAbort = makeAbort;
exports.makeProcess = makeProcess;
/* Policy Not a pure module */
