import React from 'react';
import withWidth, { isWidthUp } from '@material-ui/core/withWidth';

class WithWidthNative extends React.Component {
    render () {
        console.log(this.props);
        if (isWidthUp(this.props.breakPoint, this.props.width)) {
            return this.props.afterBreak;
        }
        return this.props.beforeBreak;
    }
}
export default withWidth()(WithWidthNative);
