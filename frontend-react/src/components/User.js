import React, { Component } from 'react';
import { Paper, Grid, Avatar, Typography, withStyles } from '@material-ui/core';
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

@inject('UserStore') @observer
class User extends Component {

	componentWillMount(){
        const { UserStore } = this.props;
        UserStore.pullUser(this.props.match.params.username);
	}

	render() {
		const { classes } = this.props;
		if (this.props.UserStore.user === undefined)
		    return null;
        const { avatar, bio, fname, lname, uname } = this.props.UserStore.user;
		console.log(this.props.UserStore);
		return (
			<main className={classes.layout}>
				<Paper className={classes.paper}>
					<Grid className={classes.container} container spacing={16}>
						<Grid item>
							<Avatar className={classes.avatar} src={avatar}/>
						</Grid>
						<Grid item className={classes.mainUserInfo}>
							<Typography variant="h6">
                                {fname} {lname}
							</Typography>
							<Typography variant="subtitle2" color="textSecondary" gutterBottom>
                                {uname}
							</Typography>
							<Typography variant="subtitle1" paragraph>
                                {bio}
							</Typography>
						</Grid>
					</Grid>
				</Paper>
		  </main>	  
		);
	}
};


export default withStyles(styles)(User);