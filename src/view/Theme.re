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
      "background": {
        "default": "#FFFFFF",
      },
    },
    "typography": {
      "fontFamily": sourceSansPro,
      "display4": {
        "fontFamily": oswald,
        "textTransform": "uppercase",
        "fontSize": "8.8vw",
        "color": "#000000",
        "letterSpacing": "normal",
        "lineHeight": "0.9",
      },
      "display1": {
        "fontSize": "1.7vw",
        "fontWeight": 600,
        "lineHeight": 1.33,
        "color": "#000000",
      },
      "headline": {
        "fontFamily": oswald,
      },
      "title": {
        "fontFamily": oswald,
        "textTransform": "uppercase",
        "fontSize": "1.875rem",
        "color": "#ffffff",
      },
      "subheading": {
        "fontFamily": oswald,
        "textTransform": "uppercase",
        "fontSize": "1.125rem",
      },
      "button": {
        "fontFamily": oswald,
        "fontSize": "1.2vw",
      },
    },
  });
