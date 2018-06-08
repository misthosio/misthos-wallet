type document;

[@bs.set] external setCookie : (document, string) => unit = "cookie";
[@bs.get] external getCookie : document => string = "cookie";

[@bs.val] external doc : document = "document";

let set = (key, value, domain) =>
  setCookie(doc, key ++ "=" ++ value ++ ";domain=" ++ domain);

let get = key =>
  Js.Re.fromString("(^| )" ++ key ++ "=([^;]+)")
  |> Js.Re.exec(getCookie(doc))
  |> Utils.andThen(result =>
       Js.Re.captures(result)[2] |> Js.Nullable.toOption
     );

let delete = (key, domain) =>
  setCookie(
    doc,
    key ++ "=;domain=" ++ domain ++ ";expires=Thu, 01 Jan 1970 00:00:01 GMT",
  );
