type t;

[@bs.module "material-ui/styles"]
external createMuiTheme : Js.t({..}) => t = "";

external toJsUnsafe : t => Js.t({..}) = "%identity";

let sourceSansPro = {|"Source Sans Pro", sans-serif|};

let oswald = {|"Oswald", sans-serif|};

let theme =
  createMuiTheme({
    "palette": {
      "primary": {
        "main": "#BEF6E9",
      },
    },
    "typography": {
      "fontFamily": sourceSansPro,
      "display4": {
        "fontFamily": oswald,
      },
      "headline": {
        "fontFamily": oswald,
      },
      "title": {
        "fontFamily": oswald,
        "color": "#ffffff",
        "textTransform": "uppercase",
        "fontSize": "30px",
        "fontWeight": "bold",
        "fontStyle": "normal",
        "fontStretch": "normal",
        "lineHeight": "1",
        "letterSpacing": "0.7px",
      },
      "button": {
        "fontFamily": oswald,
      },
    },
  });
