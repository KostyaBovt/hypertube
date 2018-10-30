import React, { Component } from 'react';
import { Paper, Grid, Avatar, Typography, withStyles } from '@material-ui/core';

const styles = theme => ({
	layout: {
		width: 'auto',
		marginLeft: theme.spacing.unit * 2,
		marginRight: theme.spacing.unit * 2,
		[theme.breakpoints.up(600 + theme.spacing.unit * 2 * 2)]: {
			width: 600,
			marginLeft: 'auto',
			marginRight: 'auto',
		}
	},
	paper: {
		marginTop: theme.spacing.unit * 3,
		marginBottom: theme.spacing.unit * 3,
		padding: theme.spacing.unit * 2
	},
	container : {
		display: 'flex',
		flexDirection: 'column',
		alignItems: 'center'
	},
	avatar: {
		margin: theme.spacing.unit,
		backgroundColor: theme.palette.secondary.main,
		width: 256,
		height: 256
	},
	mainUserInfo: {
		margin: theme.spacing.unit
	},
	editButton: {
		marginLeft: theme.spacing.unit,
	},
});

class User extends Component {
	render() {
		const { classes } = this.props;
		return (
			<main className={classes.layout}>
				<Paper className={classes.paper}>
					<Grid className={classes.container} container spacing={16}>
						<Grid item>
							<Avatar className={classes.avatar} src="https://images.unsplash.com/photo-1521119989659-a83eee488004"/>
						</Grid>
						<Grid item className={classes.mainUserInfo}>
							<Typography variant="h6">
								Roman Tarasenko
							</Typography>
							<Typography variant="subtitle2" color="textSecondary" gutterBottom>
								rtarasen
							</Typography>
							<Typography variant="subtitle1" paragraph>
								Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.
							</Typography>
						</Grid>
					</Grid>
				</Paper>
		  </main>	  
		);
	}
};


export default withStyles(styles)(User);