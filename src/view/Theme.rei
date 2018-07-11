type t;

let toJsUnsafe: t => Js.t({..});

let sourceSansPro: string;

let oswald: string;

let space: int => int;

let theme: (~dark: bool=?, unit) => t;
