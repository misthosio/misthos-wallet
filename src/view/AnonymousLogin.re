include ViewCommon;

let component = ReasonReact.statelessComponent("PublicHome");

let make = _children => {
  ...component,
  render: _self =>
    <Grid
      title1=("Anonymous Login detected" |> text)
      area3={
        <div>
          <MTypography variant=`Body2>
            (
              {js|
             You have signed in with a blockstack user that doesn't have a registered blockstack.id,
             make sure to that your blockstack-browser is up to date and that you log in with a registered id (ie. "username.blockstack.id" NOT "ID-1NVqLfofzHgraUVSf6s3k96KHG3VEudo2s"). Close all Misthos tabs (except this one) and then click the button bellow to try again.
             |js}
              |> text
            )
          </MTypography>
          <MButton color=`Inherit onClick=(_ => Session.signOut() |> ignore)>
            ("Sign Out to try again" |> text)
          </MButton>
        </div>
      }
    />,
};
