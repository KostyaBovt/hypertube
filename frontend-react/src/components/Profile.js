import React, { Component } from 'react';
import { Grid, Paper, withStyles, Avatar, List, ListSubheader, ListItem, ListItemText, Dialog, DialogTitle, DialogContent, DialogActions, Button, FormControl, InputLabel, Input, FormHelperText } from '@material-ui/core';
import { inject, observer } from 'mobx-react';
import simpleValidator from '../helpers/simpleValidator';

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
	listSection: {
		backgroundColor: 'inherit',
	},
	ul: {
		backgroundColor: 'inherit',
		padding: 0,
	},
});

const fieldLabels = {
	"fname": "First name",
	"lname": "Last name",
	"uname": "Username",
	"email": "Email",
	"bio": "Bio",
}

@inject('UserStore') @observer
class Profile extends Component {
	constructor(props) {
		super(props);
		this.state = {
			isLoading: false,
			isDialogOpen: false,
			selectedField: "fname",
			inputValue: '',
			inputError: '',

		}

		this.handleDialogClose = this.handleDialogClose.bind(this);
		this.handleItemClick = this.handleItemClick.bind(this);
		this.handleInput = this.handleInput.bind(this);
		this.saveFieldValue = this.saveFieldValue.bind(this);
	}

	handleDialogClose(e, reason) {
		this.setState({
			isDialogOpen: false
		});
	}

	handleItemClick(e) {
		const { self } = this.props.UserStore;
		const selectedField = e.currentTarget.id;

		this.setState({
			selectedField,
			isDialogOpen: true,
			inputValue: self[selectedField]
		});
	}

	handleInput(e) {
		this.setState({
			inputValue: e.target.value,
			inputError: ''
		});
	}

	handleFormSubmit(e) {
		e.preventDefault();
		this.saveFieldValue();
	}

	async saveFieldValue(e) {
		const { selectedField, inputValue } = this.state;
		const validationResult = simpleValidator(selectedField, inputValue);

		if (validationResult.isValid) {
			this.setState({ isLoading: true });
			await this.props.UserStore.updateProfile(selectedField, inputValue);
			this.setState({
				isLoading: false,
				isDialogOpen: false
			});
		} else {
			this.setState({ inputError: validationResult.error });
		}
	}
	
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
		const { isDialogOpen, selectedField, inputError, inputValue } = this.state;

		return (
			<main className={classes.layout}>

				<Dialog
					open={isDialogOpen}
					onClose={this.handleDialogClose}
					aria-labelledby="form-dialog-title"
					fullWidth
				>
					<DialogTitle id="form-dialog-title">Change {fieldLabels[selectedField].toLowerCase()}</DialogTitle>
					<DialogContent>
						<form onSubmit={this.handleFormSubmit.bind(this)}>
							<FormControl error={!!inputError} fullWidth>
								<InputLabel htmlFor={selectedField}>{fieldLabels[selectedField]}</InputLabel>
								<Input
									id={selectedField}
									type="text"
									name={selectedField}
									value={inputValue || ''}
									autoFocus
									onChange={this.handleInput}
									multiline={selectedField === 'bio'}
								/>
								<FormHelperText>{inputError}</FormHelperText>
							</FormControl>
						</form>
					</DialogContent>
					<DialogActions>
						<Button onClick={this.handleDialogClose} color="primary">
							Cancel
						</Button>
						<Button onClick={this.saveFieldValue} color="primary">
							Save
						</Button>
					</DialogActions>
				</Dialog>

				<Paper className={classes.paper}>
					<Grid className={classes.container} container spacing={16}>
						<Grid className={classes.avatarContainer} xs={12} item>
							{ this.renderAvatar(self, classes) }
						</Grid>
						<Grid item xs={12} className={classes.mainUserInfo}>
							<List subheader={ <ListSubheader color="primary">Profile</ListSubheader> }>
								<ListItem id="fname" button divider onClick={this.handleItemClick}>
									<ListItemText
										primary={self.fname}
										secondary="First name"/>
								</ListItem>
								<ListItem id="lname" button divider onClick={this.handleItemClick}>
									<ListItemText
										primary={self.lname}
										secondary="Last name"/>
								</ListItem>
								<ListItem id="uname" button divider onClick={this.handleItemClick}>
									<ListItemText
										primary={self.uname}
										secondary="Username"/>
								</ListItem>
								<ListItem id="email" button divider onClick={this.handleItemClick}>
									<ListItemText
										primary={self.email}
										secondary="Email"/>
								</ListItem>
								<ListItem id="bio" button onClick={this.handleItemClick}>
									<ListItemText
										primary={self.bio || "None"}
										secondary="Bio"
									/>
								</ListItem>
							</List>
						</Grid>
					</Grid>
				</Paper>

		  </main>	  
		);
	}
};


export default withStyles(styles)(Profile);