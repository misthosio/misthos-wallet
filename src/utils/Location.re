[@bs.val] external origin : string = "location.origin";

[@bs.val] external hostname : string = "location.hostname";

[@bs.val] external pathname : string = "location.pathname";

[@bs.val] external search : string = "location.search";

[@bs.val] external replace : string => unit = "location.replace";
