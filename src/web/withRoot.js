import React from 'react';
import PropTypes from 'prop-types';
import { MuiThemeProvider } from '@material-ui/core/styles';
import CssBaseline from '@material-ui/core/CssBaseline';
import getPageContext from './getPageContext';
import JssProvider from 'react-jss/lib/JssProvider';
import jss from './jss-insertion-point.js';

function withRoot(Component) {
  class WithRoot extends React.Component {
    constructor(props) {
      super(props);

      this.pageContext = this.props.pageContext || getPageContext();
    }

    componentDidMount() {
      // Remove the server-side injected CSS.
      const jssStyles = document.querySelector('#jss-server-side');
      if (jssStyles && jssStyles.parentNode) {
        // jssStyles.parentNode.removeChild(jssStyles);
      }
      const glamourStyles = document.querySelector('#glamour-css');
      if (glamourStyles && glamourStyles.parentNode) {
        glamourStyles.parentNode.removeChild(glamourStyles);
      }
    }

    pageContext = null;

    render() {
      // MuiThemeProvider makes the theme available down the React tree thanks to React context.
      return (
      <JssProvider>
        <MuiThemeProvider
          theme={this.pageContext.theme}
          sheetsManager={this.pageContext.sheetsManager}
        >
          <CssBaseline>
            <Component {...this.props} />
          </CssBaseline>
        </MuiThemeProvider>
      </JssProvider>
      );
    }
  }

  WithRoot.propTypes = {
    pageContext: PropTypes.object,
  };

  WithRoot.getInitialProps = ctx => {
    if (Component.getInitialProps) {
      return Component.getInitialProps(ctx);
    }

    return {};
  };

  return WithRoot;
}

export default withRoot;
