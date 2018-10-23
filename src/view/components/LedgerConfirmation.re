open Belt;
include ViewCommon;

let component = ReasonReact.statelessComponent("LedgerConfirmation");
let make =
    (
      ~action,
      ~onCancel,
      ~summary: PayoutTransaction.summary,
      ~misthosFeeAddress,
      ~changeAddress,
      ~cmdStatus,
      _children,
    ) => {
  ...component,
  render: _ => {
    let destRows =
      summary.destinations
      |. List.mapWithIndexU((. idx, (address, amount)) =>
           MaterialUi.(
             <TableRow key=(string_of_int(idx))>
               <TableCell> (address |> text) </TableCell>
               <TableCell>
                 ((amount |> BTC.format) ++ " BTC" |> text)
               </TableCell>
               <TableCell> ("RECIPIENT ADDRESS" |> text) </TableCell>
             </TableRow>
           )
         )
      |> List.toArray
      |> ReasonReact.array;
    <Grid
      title1=("Awaiting Ledger Confirmation" |> text)
      area3={
        <div>
          <MTypography variant=`Body2>
            "You will need to individually approve the following outputs:"
          </MTypography>
          <MTypography variant=`Body2>
            MaterialUi.(
              <Table>
                <TableHead>
                  <TableRow>
                    <TableCell> ("OUTPUT ADDRESS" |> text) </TableCell>
                    <TableCell> ("AMOUNT" |> text) </TableCell>
                    <TableCell> ("DESCRIPTION" |> text) </TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  destRows
                  (
                    switch (misthosFeeAddress, summary.misthosFee) {
                    | (Some(address), fee) when fee |. BTC.gt(BTC.zero) =>
                      <TableRow key="misthos-fee">
                        <TableCell> (address |> text) </TableCell>
                        <TableCell>
                          (
                            (summary.misthosFee |> BTC.format) ++ " BTC" |> text
                          )
                        </TableCell>
                        <TableCell> ("MISTHOS FEE" |> text) </TableCell>
                      </TableRow>
                    | _ => ReasonReact.null
                    }
                  )
                  (
                    switch (changeAddress) {
                    | Some((address: Address.t)) =>
                      <TableRow key="change-address">
                        <TableCell>
                          (address.displayAddress |> text)
                        </TableCell>
                        <TableCell>
                          (
                            (
                              summary.reserved
                              |. BTC.minus(summary.spentWithFees)
                              |> BTC.format
                            )
                            ++ " BTC"
                            |> text
                          )
                        </TableCell>
                        <TableCell> ("CHANGE ADDRESS" |> text) </TableCell>
                      </TableRow>
                    | _ => ReasonReact.null
                    }
                  )
                </TableBody>
              </Table>
            )
          </MTypography>
          <CommandExecutor.Status cmdStatus action />
          <MButton variant=Flat onClick=(_e => onCancel())>
            (text("Cancel"))
          </MButton>
        </div>
      }
    />;
  },
};
