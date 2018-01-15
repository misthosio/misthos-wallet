let andThenGetEvent = (watcher: option(Watcher.t)) => watcher |> Js.Option.andThen([@bs] (w => w#pendingEvent()));
let boolToJsBoolean = Js.Option.map([@bs] (b => Js.Boolean.to_js_boolean(b)));
