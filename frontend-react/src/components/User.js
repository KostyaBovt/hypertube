import React, { Component } from 'react';
import { Paper, Grid, Avatar, Typography, withStyles } from '@material-ui/core';
import { inject, observer } from 'mobx-react';
import NotFound from "./NotFound";

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
	},
	avatar: {
		margin: theme.spacing.unit,
		backgroundColor: theme.palette.grey,
		width: 256,
		height: 256
	},
	avatarContainer : {
		display: 'flex',
		justifyContent: 'center'
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

	renderAvatar(user, classes) {
		if (user.avatar) {
			return <Avatar className={classes.avatar} src={`http://localhost:8080${user.avatar}`} />;
		} else {
			return (
				<Avatar className={classes.avatar} src={user.avatar} >
					{`${user.fname.charAt(0)}${user.lname.charAt(0)}`}
				</Avatar>
			);
		}
	}

	render() {
		const { classes } = this.props;
		const { user } = this.props.UserStore;
		
		if (user === undefined) {
		    return null;
		}

		if (user === null) {
			return (<NotFound></NotFound>);
		}

		return (
			<main className={classes.layout}>
				<Paper className={classes.paper}>
					<Grid className={classes.container} container spacing={16}>
						<Grid className={classes.avatarContainer} item>
							{ this.renderAvatar(user, classes) }
						</Grid>
						<Grid item className={classes.mainUserInfo}>
							<Typography variant="h6">
                                {user.fname} {user.lname}
							</Typography>
							<Typography variant="subtitle2" color="textSecondary" gutterBottom>
                                {user.uname}
							</Typography>
							{
								user.bio &&
								<Typography variant="subtitle1" paragraph>
									{user.bio}
								</Typography>
							}
						</Grid>
					</Grid>
				</Paper>
		    </main>	  
		);
	}
};


export default withStyles(styles)(User);