module Selection = {
  type t;
  [@bs.module "d3-selection"] external make : string => t = "select";
  [@bs.send.pipe : t] external selectAll : string => t = "";
  [@bs.send.pipe : t] external append : string => t = "";
  [@bs.send.pipe : t] external data : array(Js.t({..})) => t = "";
  [@bs.send.pipe : t] external enter : unit => t = "";
  [@bs.send.pipe : t] external attr : (string, 'a) => t = "";
  [@bs.send.pipe : t] external style : (string, 'a) => t = "";
  [@bs.send.pipe : t] external text : 'a => t = "";
};

module Force = {
  type t;
  module Simulation = {
    type force = t;
    type t;
    [@bs.module "d3-force"]
    external make : array(Js.t({..})) => t = "forceSimulation";
    [@bs.send.pipe : t]
    external force :
      (
        string,
        [<
          | `ManyBody(force)
          | `Link(force)
          | `Center(force)
          | `PositionX(force)
          | `PositionY(force)
        ]
      ) =>
      t =
      "";
    [@bs.send.pipe : t] external on : (string, unit => unit) => t = "";
  };
  module ManyBody = {
    type force = t;
    type t = [ | `ManyBody(force)];
    [@bs.module "d3-force"] external make : unit => t = "forceManyBody";
    [@bs.send.pipe : t] external strength : 'a => t = "";
    [@bs.send.pipe : t] external distanceMin : int => t = "";
  };
  module Link = {
    type force = t;
    type t = [ | `Link(force)];
    [@bs.module "d3-force"]
    external make : array(Js.t({..})) => t = "forceLink";
    [@bs.send.pipe : t] external id : (Js.t({..}) => 'b) => t = "";
    [@bs.send.pipe : t] external distance : 'a => t = "";
  };
  module Center = {
    [@bs.module "d3-force"]
    external make : (int, int) => [ | `Center(t)] = "forceCenter";
  };
  module PositionX = {
    type force = t;
    type t = [ | `PositionX(force)];
    [@bs.module "d3-force"]
    external make : int => [ | `PositionX(force)] = "forceX";
    [@bs.send.pipe : t] external strength : 'a => t = "";
  };
  module PositionY = {
    [@bs.module "d3-force"]
    external make : int => [ | `PositionY(t)] = "forceY";
  };
};
