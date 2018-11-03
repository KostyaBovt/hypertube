import React, { Component } from 'react';
import { Grid, Paper, withStyles, Avatar, List, ListSubheader, ListItem, ListItemText, Dialog, DialogTitle, DialogContent, DialogActions, Button, FormControl, InputLabel, Input, FormHelperText, Icon, ListItemIcon, Snackbar, IconButton } from '@material-ui/core';
import { inject, observer } from 'mobx-react';
import simpleValidator from '../helpers/simpleValidator';
import imgHelpers from '../helpers/imgHelpers';


const styles = theme => ({
	layout: {
		width: 'auto',
		[theme.breakpoints.up(600 + theme.spacing.unit * 2 * 2)]: {
			width: 600,
			marginLeft: 'auto',
			marginRight: 'auto',
		}
	},
	paper: {
		marginTop: theme.spacing.unit * 3,
		marginBottom: theme.spacing.unit * 2,
		padding: 0
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
	}
});

const fieldLabels = {
	"fname": "First name",
	"lname": "Last name",
	"uname": "Username",
	"email": "Email",
	"bio": "Bio",
	"picture": "Profile picture"
}

@inject('SelfStore') @observer
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
		this.importPictureFromSocial = this.importPictureFromSocial.bind(this);
		this.deleteCurrentPicture = this.deleteCurrentPicture.bind(this);
		this.onFileChange = this.onFileChange.bind(this);
		this.handleSnackbarClose = this.handleSnackbarClose.bind(this);
		this.fileInput = React.createRef();
	}

	handleDialogClose(e, reason) {
		this.setState({
			isDialogOpen: false
		});
	}

	handleSnackbarClose(e, reason) {
		this.props.SelfStore.setError('');
	}

	handleItemClick(e) {
		const { self } = this.props.SelfStore;
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
		const { self } = this.props.SelfStore;
		let { selectedField, inputValue } = this.state;

		if (inputValue === null) {
			return
		}

		inputValue = inputValue.trim();
		if (inputValue === self[selectedField]) {
			this.setState({ isDialogOpen: false });
			return;
		}

		const validationResult = simpleValidator(selectedField, inputValue);

		if (validationResult.isValid) {
			this.setState({ isLoading: true });
			await this.props.SelfStore.updateProfile(selectedField, inputValue);
			this.setState({
				isLoading: false,
				isDialogOpen: false
			});
		} else {
			this.setState({ inputError: validationResult.error });
		}
	}

	async importPictureFromSocial(e) {
		const { SelfStore } = this.props;
		SelfStore.importPictureFromSocial();
	}
	
	async deleteCurrentPicture(e) {
		const { SelfStore } = this.props;
		await SelfStore.updateProfile('avatar', '');
	}
	
	async onFileChange(e) {
		const { SelfStore } = this.props;

		const file = e.target.files[0];
		e.target.value = '';

		if (file && file.size < 3000000) {
			try {
				const src = await imgHelpers.imgFileToBase64(file);
				SelfStore.updateProfile('avatar', src);
			} catch (e) {
				SelfStore.setError('image processing failed, please try again');
				console.error(e);
			}
		} else {
			SelfStore.setError('file is too big, max size is 3MB');
		}
	}

	renderDialog() {
		const { isDialogOpen, selectedField, inputError, inputValue } = this.state;

		return (
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
		);
	}

	renderSnackBar(error) {
		return (
			<Snackbar
				anchorOrigin={{
					vertical: 'bottom',
					horizontal: 'left',
				}}
				open={!!error}
				autoHideDuration={6000}
				onClose={this.handleSnackbarClose}
				message={<span>Error: {error}</span>}
				action={[
					<IconButton
						key="close"
						aria-label="Close"
						color="inherit"
						onClick={this.handleSnackbarClose}
					>
						<Icon>
							close
						</Icon>
					</IconButton>
				]}
			/>
		)
	}
	
	renderAvatar(self, classes) {
		if (self.avatar) {
			return <Avatar className={classes.avatar} src={`http://localhost:8080${self.avatar}`} />;
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
		const { self, profileError } = this.props.SelfStore;

		return (
			<main className={classes.layout}>

				{ this.renderDialog() }
				<input style={{ 'display': 'none' }} onChange={this.onFileChange} ref={this.fileInput} type="file" accept="image/*" />

				{ this.renderSnackBar(profileError) }

				<Grid className={classes.container} container spacing={0}>
					<Grid item xs={12}>
						<Paper className={classes.paper}>
							<List disablePadding subheader={ <ListSubheader disableSticky color="primary">Picture</ListSubheader> }>
								<ListItem divider className={classes.avatarContainer}>
									{ this.renderAvatar(self, classes) }
								</ListItem>
								<ListItem divider button onClick={() => { this.fileInput.current.click() }}>
									<ListItemIcon>
										<Icon>
											cloud_upload
										</Icon>
									</ListItemIcon>
									<ListItemText primary="Upload new picture"/>
								</ListItem>
								{
									self.social_provider &&
									<ListItem divider button onClick={this.importPictureFromSocial}>
										<ListItemIcon>
											<Icon>
												get_app
											</Icon>
										</ListItemIcon>
										<ListItemText primary={`Import picture from ${self.social_provider}`}/>
									</ListItem>
								}
								<ListItem button onClick={this.deleteCurrentPicture}>
									<ListItemIcon>
										<Icon>
											delete
										</Icon>
									</ListItemIcon>
									<ListItemText primary="Delete current picture"/>
								</ListItem>
							</List>
						</Paper>
					</Grid>
					<Grid item xs={12}>
						<Paper className={classes.paper}>
							<List disablePadding subheader={ <ListSubheader disableSticky color="primary">Info</ListSubheader> }>
								<ListItem id="fname" button divider onClick={this.handleItemClick}>
									<ListItemText
										primary={self.fname || "None"}
										secondary="First name"/>
								</ListItem>
								<ListItem id="lname" button divider onClick={this.handleItemClick}>
									<ListItemText
										primary={self.lname || "None"}
										secondary="Last name"/>
								</ListItem>
								<ListItem id="uname" button divider onClick={this.handleItemClick}>
									<ListItemText
										primary={self.uname}
										secondary="Username"/>
								</ListItem>
								<ListItem id="email" button divider onClick={this.handleItemClick}>
									<ListItemText
										primary={self.email || "None"}
										secondary="Email"/>
								</ListItem>
								<ListItem id="bio" button onClick={this.handleItemClick}>
									<ListItemText
										primary={self.bio || "None"}
										secondary="Bio"
									/>
								</ListItem>
							</List>
						</Paper>
					</Grid>
				</Grid>

		  </main>	  
		);
	}
};


export default withStyles(styles)(Profile);