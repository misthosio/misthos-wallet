[@bs.val] external userAgent : string = "navigator.userAgent";

let iOSCheck = [%bs.re "/(iPhone|iPad)/i"];
let androidCheck = [%bs.re "/Android/i"];

let isMobile = () =>
  Js.Re.test(userAgent, iOSCheck) || Js.Re.test(userAgent, androidCheck);
