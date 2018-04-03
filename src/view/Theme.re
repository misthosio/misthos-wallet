[@bs.module "material-ui/styles"]
external createMuiTheme : Js.t({..}) => Js.t({..}) = "createMuiTheme";

let theme = createMuiTheme({
              "palette": {
                "primary": {
                  "main": "#02A2B4",
                },
              },
            });
