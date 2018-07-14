type t;
[@bs.module "ecurve"] external getCurveByName : string => t = "";

let secp256k1 = getCurveByName("secp256k1");

module Point = {
  type curve = t;
  type t;
  [@bs.module "ecurve"] [@bs.scope "Point"]
  external decodeFrom : (curve, Node.buffer) => t = "";
  [@bs.send.pipe: t] external getEncoded : bool => Node.buffer = "";
};
