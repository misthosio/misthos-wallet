// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var React = require("react");
var ViewCommon = require("./ViewCommon.bs.js");
var Server = require("react-dom/server");

function toBase64Encoding(icon) {
  return Buffer.from(Server.renderToStaticMarkup(icon)).toString("base64");
}

function asDataUrl(icon) {
  return "data:image/svg+xml;base64," + toBase64Encoding(icon);
}

var arrowRight = React.createElement("svg", {
      height: "16",
      width: "16",
      viewBox: "0 0 16 16",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          stroke: "#000",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          strokeWidth: "2"
        }, React.createElement("path", {
              d: "M1 8h14M8 1l7 7-7 7"
            })));

var arrowUpCircle = React.createElement("svg", {
      height: "26",
      width: "26",
      viewBox: "0 0 26 26",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          stroke: "#000",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          strokeWidth: "2",
          transform: "translate(1 1)"
        }, React.createElement("circle", {
              cx: "12",
              cy: "12",
              r: "12"
            }), React.createElement("path", {
              d: "M12 17V7M7 12l5-5 5 5"
            })));

var close = React.createElement("svg", {
      height: "20",
      width: "20",
      viewBox: "0 0 20 20",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd"
        }, React.createElement("path", {
              d: "M19.333 2.533L17.467.667 10 8.133 2.533.667.667 2.533 8.133 10 .667 17.467l1.866 1.866L10 11.867l7.467 7.466 1.866-1.866L11.867 10z",
              fill: "#000"
            }), React.createElement("path", {
              d: "M26 26H-6V-6h32z"
            })));

var copy = React.createElement("svg", {
      height: "22",
      width: "22",
      viewBox: "0 0 22 22",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          stroke: "#000",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          strokeWidth: "2",
          transform: "translate(1 1)"
        }, React.createElement("rect", {
              height: "13",
              width: "13",
              rx: "2",
              x: "7",
              y: "7"
            }), React.createElement("path", {
              d: "M3 13H2a2 2 0 0 1-2-2V2a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"
            })));

var logoBig = React.createElement("svg", {
      height: "419",
      width: "584",
      viewBox: "0 0 584 419",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("defs", undefined, React.createElement("linearGradient", {
              id: "a",
              x1: "-1.816%",
              x2: "117.054%",
              y1: "69.515%",
              y2: "38.068%"
            }, React.createElement("stop", {
                  offset: "0%",
                  stopColor: "#59F7F0"
                }), React.createElement("stop", {
                  offset: "49.223%",
                  stopColor: "#02A2B4"
                }), React.createElement("stop", {
                  offset: "100%",
                  stopColor: "#067781"
                }))), React.createElement("path", {
          d: "M235.397 190.474h113.805v228.21H235.397v-228.21zM410.864.014C506.19.143 584 81.797 584 182.559v236.125H472.253V182.558c0-37.751-27.529-68.653-63.268-68.653-26.714 0-48.225 16.02-58.085 41.289H232.56c-9.854-25.27-31.101-41.289-57.815-41.289-35.729 0-62.993 30.907-62.993 68.653v236.125H0V3.593h111.752v9.683C173.04-13.47 244.61.896 293.65 50.07 325.662 17.904 366.685-.02 410.864.015z",
          fill: "url(#a)",
          fillRule: "nonzero"
        }));

var logoSolid = React.createElement("svg", {
      height: "32",
      width: "45",
      viewBox: "0 0 45 32",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("defs", undefined, React.createElement("linearGradient", {
              id: "a",
              x1: "-1.816%",
              x2: "117.054%",
              y1: "69.515%",
              y2: "38.068%"
            }, React.createElement("stop", {
                  offset: "0%",
                  stopColor: "#59F7F0"
                }), React.createElement("stop", {
                  offset: "49.223%",
                  stopColor: "#02A2B4"
                }), React.createElement("stop", {
                  offset: "100%",
                  stopColor: "#067781"
                }))), React.createElement("path", {
          d: "M41.991 30.558h8.699V48H41.99V30.558zM55.402 16c7.286.01 13.233 6.25 13.233 13.952V48h-8.54V29.953c0-2.885-2.105-5.247-4.836-5.247-2.042 0-3.686 1.224-4.44 3.155h-9.044c-.754-1.93-2.377-3.155-4.42-3.155-2.73 0-4.814 2.362-4.814 5.247V48H24V16.275h8.541v.74c4.684-2.045 10.155-.946 13.903 2.812 2.446-2.459 5.582-3.829 8.958-3.826z",
          fill: "url(#a)",
          fillRule: "nonzero",
          transform: "translate(-24 -16)"
        }));

var menu = React.createElement("svg", {
      height: "24",
      width: "24",
      viewBox: "0 0 24 16",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd"
        }, React.createElement("path", {
              d: "M-4-8h32v32H-4z"
            }), React.createElement("path", {
              d: "M0 16h12v-2.667H0V16zm0-6.667h24V6.667H0v2.666zM0 0v2.667h24V0H0z",
              fill: "#000"
            })));

var minusCircle = React.createElement("svg", {
      height: "26",
      width: "26",
      viewBox: "0 0 26 26",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          stroke: "#000",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          strokeWidth: "2",
          transform: "translate(1 1)"
        }, React.createElement("circle", {
              cx: "12",
              cy: "12",
              r: "12"
            }), React.createElement("path", {
              d: "M7.2 12h9.6"
            })));

var plusCircle = React.createElement("svg", {
      height: "26",
      width: "26",
      viewBox: "0 0 26 26",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          stroke: "#000",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          strokeWidth: "2",
          transform: "translate(1 1)"
        }, React.createElement("circle", {
              cx: "12",
              cy: "12",
              r: "12"
            }), React.createElement("path", {
              d: "M12 7.2v9.6M7.2 12h9.6"
            })));

var remove = React.createElement("svg", {
      height: "26",
      width: "26",
      viewBox: "0 0 26 26",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          stroke: "#000",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          strokeWidth: "2",
          transform: "translate(1 1)"
        }, React.createElement("circle", {
              cx: "12",
              cy: "12",
              r: "12"
            }), React.createElement("path", {
              d: "M15.6 8.4l-7.2 7.2M8.4 8.4l7.2 7.2"
            })));

var stepBg = React.createElement("svg", {
      height: "44",
      width: "44",
      viewBox: "0 0 44 44",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("defs", undefined, React.createElement("linearGradient", {
              id: "a",
              x1: "162.467%",
              x2: "-41.102%",
              y1: "29.557%",
              y2: "66.287%"
            }, React.createElement("stop", {
                  offset: "0%",
                  stopColor: "#05CFDB"
                }), React.createElement("stop", {
                  offset: "100%",
                  stopColor: "#02A2B4"
                }))), React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          transform: "translate(1 1)"
        }, React.createElement("circle", {
              cx: "21",
              cy: "21",
              r: "21",
              stroke: "#000"
            }), React.createElement("circle", {
              cx: "21",
              cy: "21",
              fill: "url(#a)",
              r: "18"
            })));

var blockStack = React.createElement("svg", {
      height: "24",
      width: "24",
      viewBox: "0 0 24 24",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "nonzero"
        }, React.createElement("path", {
              d: "M24 16.431s.006 2.502-.267 3.586c-.272 1.084-.772 1.84-1.326 2.394-.557.556-1.294 1.063-2.418 1.326-1.124.264-3.563.26-3.563.26L7.566 24s-2.502.006-3.586-.266c-1.084-.273-1.84-.773-2.395-1.327C1.03 21.851.523 21.113.26 19.99-.005 18.866 0 16.427 0 16.427V7.57s-.006-2.502.267-3.586c.272-1.084.772-1.84 1.326-2.394C2.15 1.033 2.887.526 4.011.263c1.124-.264 3.563-.26 3.563-.26L16.434 0s2.502-.006 3.586.266c1.084.273 1.84.773 2.395 1.327.556.556 1.062 1.294 1.326 2.417.264 1.124.259 3.563.259 3.563v8.858z",
              fill: "#270F34"
            }), React.createElement("g", {
              fill: "#FEFEFE"
            }, React.createElement("path", {
                  d: "M8.12 17.832a1.956 1.956 0 1 0 0-3.911 1.956 1.956 0 0 0 0 3.911M8.12 10.035a1.956 1.956 0 1 0 0-3.912 1.956 1.956 0 0 0 0 3.912M15.943 10.035a1.956 1.956 0 1 0 0-3.912 1.956 1.956 0 0 0 0 3.912M15.943 17.832a1.956 1.956 0 1 0 0-3.912 1.956 1.956 0 0 0 0 3.912"
                }))));

var misthosWordMark = React.createElement("svg", {
      height: "31",
      width: "142",
      viewBox: "0 0 142 31",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("defs", undefined, React.createElement("linearGradient", {
              id: "a",
              x1: "-1.816%",
              x2: "117.054%",
              y1: "69.515%",
              y2: "38.068%"
            }, React.createElement("stop", {
                  offset: "0%",
                  stopColor: "#59F7F0"
                }), React.createElement("stop", {
                  offset: "49.223%",
                  stopColor: "#02A2B4"
                }), React.createElement("stop", {
                  offset: "100%",
                  stopColor: "#067781"
                }))), React.createElement("path", {
          d: "M65.498 90.655a3.646 3.646 0 1 1-7.294 0 3.646 3.646 0 0 1 7.294 0zm83.55 16.575c0 5.912-4.938 10.614-11.134 10.614-6.15 0-11.087-4.702-11.087-10.614 0-5.867 4.938-10.616 11.087-10.616 6.196 0 11.134 4.749 11.134 10.616zm-5.87-.047c0-1.444-.513-2.794-1.49-3.817-1.026-1.071-2.284-1.63-3.728-1.63-1.35 0-2.701.559-3.633 1.63-1.071 1.023-1.584 2.42-1.584 3.864 0 1.489.513 2.84 1.584 3.91.932 1.024 2.283 1.583 3.633 1.583 1.444 0 2.702-.559 3.728-1.583.978-1.07 1.49-2.467 1.49-3.957zm-106.316-2.088h6.087v12.363h-6.087v-12.363zm9.518-10.4c5.192.008 9.43 4.447 9.43 9.926v12.837h-6.086v-12.837c0-2.053-1.5-3.733-3.446-3.733-1.456 0-2.627.87-3.164 2.245h-6.446c-.537-1.374-1.694-2.245-3.15-2.245-1.946 0-3.43 1.68-3.43 3.733v12.837H24V94.89h6.087v.527c3.339-1.454 7.237-.673 9.908 2 1.744-1.748 3.978-2.723 6.385-2.721zm12.564 22.754V97.661h5.663v19.788h-5.663zm11.677-6.749c2.765 3.306 6.98 3.444 6.98.964 0-1.47-1.361-1.929-2.766-2.388-3.688-1.193-6.234-2.387-6.234-6.197 0-3.628 3.117-6.015 6.717-6.015 3.249 0 5.005.734 7.55 2.341l-2.546 4.178c-3.687-2.754-6.189-2.525-6.453-.413-.219 1.47 1.844 1.745 4.04 2.525 2.677.827 5.179 2.71 5.179 6.107 0 3.168-3.116 6.198-6.98 6.198-3.424 0-6.672-1.147-8.999-3.58l3.512-3.72zm82.912 0c2.766 3.306 6.98 3.444 6.98.964 0-1.47-1.36-1.929-2.766-2.388-3.687-1.193-6.233-2.387-6.233-6.197 0-3.628 3.117-6.015 6.717-6.015 3.248 0 5.004.734 7.55 2.341l-2.546 4.178c-3.687-2.754-6.19-2.525-6.453-.413-.22 1.47 1.843 1.745 4.038 2.525 2.678.827 5.18 2.71 5.18 6.107 0 3.168-3.116 6.198-6.98 6.198-3.424 0-6.673-1.147-8.999-3.58l3.512-3.72zm-58.45-13.062h4.32v5.299h-4.32v14.521H89.39v-14.521h-3.927v-5.3h3.927V87.097h5.694v10.542zm19.083-.573c5.53 0 10.008 4.729 10.009 10.513v9.872h-5.795v-9.872a5.77 5.77 0 0 0-1.449-3.81c-.921-1.01-2.15-1.561-3.467-1.561-1.317 0-2.59.55-3.513 1.56a5.767 5.767 0 0 0-1.448 3.811v9.872h-5.839V87h5.839v11.9c1.625-1.147 3.556-1.835 5.663-1.835z",
          fill: "url(#a)",
          fillRule: "nonzero",
          transform: "translate(-24 -87)"
        }));

var twitter = React.createElement("svg", {
      height: "16",
      width: "20",
      viewBox: "0 0 20 16",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("path", {
          d: "M17.8 3.987c.013.175.013.35.013.525C17.813 9.85 13.722 16 6.244 16A11.55 11.55 0 0 1 0 14.188c.327.037.642.05.982.05 1.9 0 3.65-.638 5.048-1.725a4.07 4.07 0 0 1-3.802-2.8c.252.037.504.062.768.062.365 0 .73-.05 1.07-.137a4.045 4.045 0 0 1-3.26-3.963v-.05c.541.3 1.17.488 1.838.512a4.027 4.027 0 0 1-1.26-5.4A11.585 11.585 0 0 0 9.77 4.962c-.063-.3-.1-.612-.1-.924C9.668 1.812 11.48 0 13.733 0a4.07 4.07 0 0 1 2.971 1.275A8.04 8.04 0 0 0 19.286.3a4.032 4.032 0 0 1-1.788 2.225A8.197 8.197 0 0 0 19.84 1.9a8.707 8.707 0 0 1-2.04 2.087z",
          fill: "#FFF",
          fillRule: "nonzero"
        }));

var linkedin = React.createElement("svg", {
      height: "16",
      width: "16",
      viewBox: "0 0 16 16",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("path", {
          d: "M3.2 16H0V5.12h3.2V16zM1.92 3.84C.86 3.84 0 2.966 0 1.911 0 .856.86 0 1.92 0s1.92.856 1.92 1.911c0 1.055-.86 1.929-1.92 1.929zM15.996 16H12.72v-5.167c0-1.231-.025-2.81-1.708-2.81-1.708 0-1.97 1.338-1.97 2.722V16H5.76V5.386h3.15v1.448h.046C9.395 6 10.466 5.12 12.065 5.12 15.388 5.12 16 7.317 16 10.17V16h-.004z",
          fill: "#FFF",
          fillRule: "nonzero"
        }));

var medium = React.createElement("svg", {
      height: "16",
      width: "20",
      viewBox: "0 0 20 16",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("path", {
          d: "M19.337.816l-1.52 1.469a.452.452 0 0 0-.171.428v10.816a.442.442 0 0 0 .171.428l1.488 1.47v.325h-7.473v-.313l1.539-1.508c.152-.153.152-.198.152-.428V4.757L9.24 15.72h-.576L3.681 4.757v7.347c-.045.307.063.62.278.844l2.002 2.446v.326H.273v-.326l2.001-2.446a.984.984 0 0 0 .26-.844V3.607a.733.733 0 0 0-.24-.626L.513.816V.49h5.529l4.269 9.455L14.068.496h5.27v.32z",
          fill: "#FFF",
          fillRule: "nonzero"
        }));

var chevronDown = React.createElement("svg", {
      height: "8",
      width: "14",
      viewBox: "0 0 14 8",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("path", {
          d: "M1 1l6 6 6-6",
          fill: "none",
          fillRule: "evenodd",
          stroke: "#000",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          strokeWidth: "2"
        }));

var arrowDownBig = React.createElement("svg", {
      height: "45",
      width: "45",
      viewBox: "0 0 45 45",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          stroke: "#000",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          transform: "translate(1 1)"
        }, React.createElement("circle", {
              cx: "21.5",
              cy: "21.5",
              fill: "#FFF",
              r: "21.5"
            }), React.createElement("path", {
              d: "M12.9 18.992l8.6 8.6 8.6-8.6"
            })));

var avatar = React.createElement("svg", {
      height: "70",
      width: "70",
      viewBox: "0 0 70 70",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("defs", undefined, React.createElement("linearGradient", {
              id: "b",
              x1: "-1.146%",
              y1: "71.336%",
              y2: "0%"
            }, React.createElement("stop", {
                  offset: "0%",
                  stopColor: "#59F7F0"
                }), React.createElement("stop", {
                  offset: "28.22%",
                  stopColor: "#02A2B4"
                }), React.createElement("stop", {
                  offset: "56.765%",
                  stopColor: "#067781"
                }), React.createElement("stop", {
                  offset: "79.931%",
                  stopColor: "#FF006D"
                }), React.createElement("stop", {
                  offset: "100%",
                  stopColor: "#F65E25"
                }))), React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          transform: "translate(6 6)"
        }, React.createElement("ellipse", {
              cx: "29",
              cy: "28.642",
              fill: "#000",
              fillOpacity: ".98",
              rx: "29",
              ry: "28.642"
            }), React.createElement("ellipse", {
              cx: "29",
              cy: "28.642",
              rx: "32",
              ry: "31.642",
              stroke: "url(#b)",
              strokeWidth: "6"
            }), React.createElement("ellipse", {
              cx: "29",
              cy: "28.642",
              rx: "31",
              ry: "30.642",
              stroke: "#FFF",
              strokeWidth: "4"
            })));

var send = React.createElement("svg", {
      version: "1.1",
      viewBox: "0 0 18 18",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          strokeLinecap: "round",
          strokeLinejoin: "round"
        }, React.createElement("g", {
              stroke: "#000",
              strokeWidth: "2",
              transform: "translate(-401 -651)"
            }, React.createElement("g", {
                  transform: "translate(190 560)"
                }, React.createElement("g", {
                      transform: "translate(212 92)"
                    }, React.createElement("path", {
                          d: "m16 0l-8.8 8.8"
                        }), React.createElement("polygon", {
                          points: "16 0 10.4 16 7.2 8.8 0 5.6"
                        }))))));

var clock = React.createElement("svg", {
      height: "22",
      width: "22",
      viewBox: "0 0 22 22",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          stroke: "currentColor",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          strokeWidth: "2",
          transform: "translate(1 1)"
        }, React.createElement("circle", {
              cx: "10",
              cy: "10",
              r: "10"
            }), React.createElement("path", {
              d: "M10 4v6l4 2"
            })));

var settings = React.createElement("svg", {
      height: "22",
      width: "22",
      viewBox: "0 0 22 22",
      xmlns: "http://www.w3.org/2000/svg"
    }, React.createElement("g", {
          fill: "none",
          fillRule: "evenodd",
          stroke: "currentColor",
          strokeLinecap: "round",
          strokeLinejoin: "round",
          strokeWidth: "2",
          transform: "translate(1 1)"
        }, React.createElement("circle", {
              cx: "10",
              cy: "10",
              r: "2.727"
            }), React.createElement("path", {
              d: "M16.727 12.727a1.5 1.5 0 0 0 .3 1.655l.055.054a1.818 1.818 0 1 1-2.573 2.573l-.054-.054a1.5 1.5 0 0 0-1.655-.3 1.5 1.5 0 0 0-.91 1.372v.155a1.818 1.818 0 1 1-3.635 0V18.1a1.5 1.5 0 0 0-.982-1.373 1.5 1.5 0 0 0-1.655.3l-.054.055a1.818 1.818 0 1 1-2.573-2.573l.054-.054a1.5 1.5 0 0 0 .3-1.655 1.5 1.5 0 0 0-1.372-.91h-.155a1.818 1.818 0 1 1 0-3.635H1.9a1.5 1.5 0 0 0 1.373-.982 1.5 1.5 0 0 0-.3-1.655l-.055-.054A1.818 1.818 0 1 1 5.491 2.99l.054.054a1.5 1.5 0 0 0 1.655.3h.073a1.5 1.5 0 0 0 .909-1.372v-.155a1.818 1.818 0 0 1 3.636 0V1.9a1.5 1.5 0 0 0 .91 1.373 1.5 1.5 0 0 0 1.654-.3l.054-.055a1.818 1.818 0 1 1 2.573 2.573l-.054.054a1.5 1.5 0 0 0-.3 1.655v.073a1.5 1.5 0 0 0 1.372.909h.155a1.818 1.818 0 1 1 0 3.636H18.1a1.5 1.5 0 0 0-1.373.91z"
            })));

var text = ViewCommon.text;

var extractString = ViewCommon.extractString;

var ignoreEvent = ViewCommon.ignoreEvent;

exports.text = text;
exports.extractString = extractString;
exports.ignoreEvent = ignoreEvent;
exports.toBase64Encoding = toBase64Encoding;
exports.asDataUrl = asDataUrl;
exports.arrowRight = arrowRight;
exports.arrowUpCircle = arrowUpCircle;
exports.close = close;
exports.copy = copy;
exports.logoBig = logoBig;
exports.logoSolid = logoSolid;
exports.menu = menu;
exports.minusCircle = minusCircle;
exports.plusCircle = plusCircle;
exports.remove = remove;
exports.stepBg = stepBg;
exports.blockStack = blockStack;
exports.misthosWordMark = misthosWordMark;
exports.twitter = twitter;
exports.linkedin = linkedin;
exports.medium = medium;
exports.chevronDown = chevronDown;
exports.arrowDownBig = arrowDownBig;
exports.avatar = avatar;
exports.send = send;
exports.clock = clock;
exports.settings = settings;
/* arrowRight Not a pure module */
