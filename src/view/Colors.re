open Css;

let black = hex("000000");

let white = hex("ffffff");

let robinsEgg = hex("59f7f0");

let misthosTeal = hex("02a2b4");

let strongPink = hex("ff006d");

let reddishOrange = hex("f65e25");

let gradient =
  linearGradient(
    deg(90),
    [
      (0, robinsEgg),
      (28, misthosTeal),
      (57, hex("067781")),
      (80, strongPink),
      (100, reddishOrange),
    ],
  );
