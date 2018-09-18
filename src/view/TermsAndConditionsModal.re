include Belt;

include ViewCommon;

let component = ReasonReact.statelessComponent("ViewIncomeModal");

let make = (~signTAC, _children) => {
  let onAggree = ignoreEvent(signTAC);
  {
    ...component,
    render: _ =>
      <Grid
        title1=("Latest Misthos Terms of Use" |> text)
        area3={
          <div className=ScrollList.containerStyles>
            <ScrollList>
              (
                TACText.terms
                |. Array.map(section =>
                     [|
                       <MTypography variant=`Subheading>
                         (section.heading |> text)
                       </MTypography>,
                       switch (section.body) {
                       | P(a) =>
                         a
                         |. Array.map(p =>
                              <MTypography gutterBottom=true variant=`Body1>
                                (p |> text)
                              </MTypography>
                            )
                         |> ReasonReact.array
                       | L(a) =>
                         <ul>
                           (
                             a
                             |. Array.map(p =>
                                  <li>
                                    <MTypography variant=`Body1>
                                      (p |> text)
                                    </MTypography>
                                  </li>
                                )
                             |> ReasonReact.array
                           )
                         </ul>
                       },
                     |]
                   )
                |> Array.concatMany
                |> ReasonReact.array
              )
            </ScrollList>
            <MButton onClick=onAggree>
              ("I agree to the terms of Use" |> text)
            </MButton>
          </div>
        }
      />,
  };
};
