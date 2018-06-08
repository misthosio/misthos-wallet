module Head = {
  [@bs.module "next/head"] external head : ReasonReact.reactClass = "default";

  let make = children =>
    ReasonReact.wrapJsForReason(~reactClass=head, ~props=None, children);
};

let default = () =>
  <div id="root">
    <Head>
      <meta charSet="utf-8" />
      <meta
        name="viewport"
        content="width=device-width, initial-scale=1, shrink-to-fit=no"
      />
      <meta name="theme-color" content="#000000" />
      <link
        rel="apple-touch-icon"
        sizes="180x180"
        href="static/apple-touch-icon.png"
      />
      <link
        rel="icon"
        type_="image/png"
        sizes="32x32"
        href="static/favicon-32x32.png"
      />
      <link
        rel="icon"
        type_="image/png"
        sizes="16x16"
        href="static/favicon-16x16.png"
      />
      <link rel="manifest" href="static/manifest.json" />
      <meta name="msapplication-TileColor" content="#ffffff" />
      <meta name="theme-color" content="#ffffff" />
      <link
        href="https://fonts.googleapis.com/css?family=Oswald:600,700|Source+Sans+Pro:300,600"
        rel="stylesheet"
      />
      <noscript id="jss-insertion-point" />
      <title> ("Misthos" |> ReasonReact.string) </title>
    </Head>
    <JssProvider>
      <Layout drawer=None modal=None>
        <PublicHome onSignIn=(_ => ()) />
      </Layout>
    </JssProvider>
  </div>;
