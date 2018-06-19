open Css;

let uDarkGray = "1f2532";

let darkGray = hex(uDarkGray);

let uBlack = "000000";

let black = hex(uBlack);

let uWhite = "FFFFFF";

let white = hex(uWhite);

let uSucces = "33d321";

let success = hex(uSucces);

let uError = "d0021b";

let error = hex(uError);

let uRobinsEgg = "59f7f0";

let robinsEgg = hex(uRobinsEgg);

let uDeepAqua = "067781";

let deepAqua = hex(uDeepAqua);

let uMisthosTeal = "02a2b4";

let misthosTeal = hex(uMisthosTeal);

let uStrongPink = "ff006d";

let strongPink = hex(uStrongPink);

let uReddishOrange = "f65e25";

let reddishOrange = hex(uReddishOrange);

let gradient =
  linearGradient(
    deg(90),
    [
      (0, robinsEgg),
      (28, misthosTeal),
      (57, deepAqua),
      (80, strongPink),
      (100, reddishOrange),
    ],
  );

let uGradient = {j|linear-gradient(90deg, #$uRobinsEgg 0%, #$uMisthosTeal 28%, #$uDeepAqua 57%, #$uStrongPink 80%, #$uReddishOrange 100%)|j};

let gradientAqua =
  linearGradient(
    deg(75),
    [(0, robinsEgg), (49, misthosTeal), (100, deepAqua)],
  );

let uGradientAqua = {j|linear-gradient(75deg, #$uRobinsEgg, #$uMisthosTeal 49%, #$uDeepAqua)|j};

let uGradientAquaLight = {j|linear-gradient(90deg, #$uRobinsEgg, #$uMisthosTeal 49%, #$uDeepAqua)|j};

let gradientOrange =
  linearGradient(deg(75), [(0, strongPink), (100, reddishOrange)]);

let uGradientOrange = {j|linear-gradient(75deg, #$uStrongPink, #$uReddishOrange)|j};
