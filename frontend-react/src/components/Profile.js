import React, { Component } from 'react';
import { Typography, Grid, Paper, withStyles, Avatar, Icon } from '@material-ui/core';
import { inject, observer } from 'mobx-react';

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
		flexDirection: 'column'
	},
	avatarContainer : {
		display: 'flex',
		justifyContent: 'center'
	},
	avatar: {
		margin: theme.spacing.unit,
		backgroundColor: theme.palette.grey,
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

@inject('UserStore') @observer
class Profile extends Component {
	renderAvatar(self, classes) {
		if (self.avatar) {
			return <Avatar className={classes.avatar} src={self.avatar} />;
		} else {
			return (
				<Avatar className={classes.avatar} src={self.avatar} >
					{`${self.fname.charAt(0)}${self.lname.charAt(0)}`}
				</Avatar>
			);
		}
	}

	render() {
		const { classes } = this.props;
		const { self } = this.props.UserStore;

		return (
			<main className={classes.layout}>
				<Paper className={classes.paper}>
					<Grid className={classes.container} container spacing={16}>
						<Grid className={classes.avatarContainer} xs={12} item>
							{ this.renderAvatar(self, classes) }
						</Grid>
						<Grid item xs={12} className={classes.mainUserInfo}>
							<Typography variant="h6">
								{`${self.fname} ${self.lname}`}
							</Typography>
							<Typography variant="subtitle2" color="textSecondary" gutterBottom>
								{ self.uname }
							</Typography>
							{
								self.bio &&
								<Typography variant="subtitle1" paragraph>
									{ self.bio }
								</Typography>
							}
						</Grid>
					</Grid>
				</Paper>
		  </main>	  
		);
	}
};


export default withStyles(styles)(Profile);