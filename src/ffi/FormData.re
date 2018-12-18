type t = Fetch.formData;

[@bs.new] external make: unit => t = "FormData";
[@bs.send] external append: (t, string, string) => unit = "";
