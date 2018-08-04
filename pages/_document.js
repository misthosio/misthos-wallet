import React from 'react';
import Document, { Head, Main, NextScript } from 'next/document';
import JssProvider from 'react-jss/lib/JssProvider';
import flush from 'styled-jsx/server';
import getPageContext from '../src/web/getPageContext';
import {renderStaticOptimized} from 'glamor/server'
import jss from '../src/web/jss-insertion-point';
import ReactDOMServer from 'react-dom/server';

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

MyDocument.getInitialProps = ctx => {
  // Resolution order
  //
  // On the server:
  // 1. page.getInitialProps
  // 2. document.getInitialProps
  // 3. page.render
  // 4. document.render
  //
  // On the server with error:
  // 2. document.getInitialProps
  // 3. page.render
  // 4. document.render
  //
  // On the client
  // 1. page.getInitialProps
  // 3. page.render

  // Get the context of the page to collected side effects.
  const pageContext = getPageContext();
  const page = ctx.renderPage(Component => props => (
    <JssProvider
      registry={pageContext.sheetsRegistry}
      generateClassName={pageContext.generateClassName}
    >
      <Component pageContext={pageContext} {...props} />
    </JssProvider>
  ));
  const glamourStyles = renderStaticOptimized(() => page.html)

  return {
    ...page,
    pageContext,
    styles: (
      <React.Fragment>
      <style
        id="jss-server-side"
        // eslint-disable-next-line react/no-danger
        dangerouslySetInnerHTML={{ __html: pageContext.sheetsRegistry.toString() }}
      />
      <style id="glamour-css"
        // eslint-disable-next-line react/no-danger
        dangerouslySetInnerHTML={{ __html: glamourStyles.css }}
      />
      {flush() || null}
      </React.Fragment>
    ),
  };
};

export default MyDocument;
