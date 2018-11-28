import Document, { Head, Main, NextScript } from 'next/document';

class MyDocument extends Document {
    render() {
        const { pageContext } = this.props;

        const ga = "(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start': new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0], j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src= 'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f); })(window,document,'script','dataLayer','GTM-PDCW5VS');"

        return (
                <html lang="en" dir="ltr">
                <Head>
                <script dangerouslySetInnerHTML={{ __html: ga }}/>
                <meta charSet="utf-8" />
                <meta
            name="viewport"
            content="width=device-width, initial-scale=1, shrink-to-fit=no"
                />
                <meta name="theme-color" content="#000000" />
                <link
            rel="apple-touch-icon"
            sizes="180x180"
            href="/static/apple-touch-icon.png"
                />
                <link
            rel="icon"
            type="image/png"
            sizes="32x32"
            href="/static/favicon-32x32.png"
                />
                <link
            rel="icon"
            type="image/png"
            sizes="16x16"
            href="/static/favicon-16x16.png"
                />
                <link rel="manifest" href="/static/manifest.json" />
                <meta name="msapplication-TileColor" content="#ffffff" />
                <meta name="theme-color" content="#ffffff" />
                <link
            href="https://fonts.googleapis.com/css?family=Oswald:600,700|Source+Sans+Pro:300,400,600"
            rel="stylesheet"
                />
                <noscript id="jss-insertion-point"></noscript>
                <title>Misthos - A multisig Bitcoin wallet for businesses</title>
                <meta name="description" content="Misthos is the most advanced multisig bitcoin wallet for businesses, emphasizing frictionless setup, low risk and streamlined collaboration." />
                </Head>
                <body>
                <Main />
                <NextScript />
                </body>
                </html>
        );
    }
}

export default MyDocument;
