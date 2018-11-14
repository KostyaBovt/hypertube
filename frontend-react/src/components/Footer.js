import React, { Component } from 'react';
import { withStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import grey from "@material-ui/core/colors/grey"

const styles = {
	footer: {
		position: 'absolute',
		bottom: '0',
		backgroundColor: '#009688;',
		width: '100%'
  },
  typography: {
	  color: grey[100]
  }
};

class Footer extends Component {
	render() {
		const { classes } = this.props;
		return (
			<footer className={classes.footer}>
				<Typography variant="subtitle2" align="center" className={classes.typography}>
					aklimchu lmalaya kbovt rtarasen
				</Typography>
				<Typography variant="subtitle2" align="center" className={classes.typography}>
					© 2018
				</Typography>
			</footer>
		);
	}
};

export default withStyles(styles)(Footer);
