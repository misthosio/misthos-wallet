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
      },
    },
  });
