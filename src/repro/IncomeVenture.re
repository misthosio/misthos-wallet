let eventLog = IncomeLog.log |> Json.parseOrRaise |> EventLog.decode;
