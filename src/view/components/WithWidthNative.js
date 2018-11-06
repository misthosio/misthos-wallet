import React from 'react';
import withWidth, { isWidthUp } from '@material-ui/core/withWidth';

class WithWidthNative extends React.Component {
    render () {
        if (isWidthUp(this.props.breakPoint, this.props.width)) {
            return this.props.beforeBreak;
        }
        return this.props.afterBreak;

    }
}
export default withWidth()(WithWidthNative);
