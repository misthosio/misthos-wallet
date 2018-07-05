type t = Fetch.formData;

[@bs.module] [@bs.new] external make : unit => t = "form-data";
[@bs.send] external append : (t, string, string) => unit = "";
