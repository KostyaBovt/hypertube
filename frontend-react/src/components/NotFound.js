import React, {Component} from 'react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import not_found from '../img/not_found.jpg';

const styles = theme => ({
notFound: {
		paddingTop: 70,
		display: 'flex',
		justifyContent: 'center'
	},
});

class NotFound extends Component {

	render() {
		const { classes } = this.props;
        return (<Grid className={classes.notFound}><img src={not_found}/></Grid>)
    }

}

export default withStyles(styles)(NotFound);