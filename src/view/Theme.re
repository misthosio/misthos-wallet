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
        "main": "#ffffff",
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
        "fontSize": "9.688vw",
        "color": "#000000",
        "letterSpacing": "normal",
        "fontWeight": 700,
        "lineHeight": "0.9",
      },
      "display3": {
        "fontSize": "7.188vw",
        "color": "#000000",
        "letterSpacing": "0.0391vw",
        "fontWeight": 600,
        "lineHeight": "0.9",
      },
      "display2": {
        "fontSize": "7.188vw",
        "color": "#000000",
        "letterSpacing": "0.0391vw",
        "fontWeight": 300,
        "lineHeight": "0.9",
      },
      "display1": {
        "fontSize": "1.875vw",
        "color": "#000000",
        "letterSpacing": "0.0391vw",
        "fontWeight": 600,
        "lineHeight": 1.33,
      },
      "headline": {
        "fontFamily": oswald,
        "textTransform": "uppercase",
        "fontSize": "2.344vw",
        "color": "#000000",
        "letterSpacing": "normal",
        "fontWeight": 700,
      },
      "title": {
        "fontFamily": oswald,
        "textTransform": "uppercase",
        "fontSize": "2.031vw",
        "color": "#ffffff",
        "letterSpacing": "normal",
        "fontWeight": 600,
      },
      "subheading": {
        "fontSize": "2.344vw",
        "color": "#000000",
        "letterSpacing": "0.0391vw",
        "fontWeight": 600,
      },
      "body2": {
        "fontSize": "1.25vw",
        "color": "#000000",
        "letterSpacing": "0.0391vw",
        "fontWeight": 600,
      },
      "body1": {
        "fontSize": "1.25vw",
        "color": "#000000",
        "letterSpacing": "0.0391vw",
        "fontWeight": 300,
      },
      "caption": {
        "fontSize": "1.094vw",
        "color": "#000000",
        "letterSpacing": "0.0391vw",
        "fontWeight": 600,
      },
      "button": {
        "fontFamily": oswald,
        "textTransform": "uppercase",
        "fontSize": "1.406vw",
        "color": "#000000",
        "letterSpacing": "0.0547vw",
        "fontWeight": 700,
        "fontSize": "1.2vw",
      },
    },
  });
