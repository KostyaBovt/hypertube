import React, { Component } from 'react';
import "../../node_modules/materialize-social/materialize-social.css";
import { inject, observer } from 'mobx-react';
import { withStyles } from '@material-ui/core/styles';
import FormControl from '@material-ui/core/FormControl';
import FormHelperText from '@material-ui/core/FormHelperText';
import Input from '@material-ui/core/Input';
import InputLabel from '@material-ui/core/InputLabel';
import Button from '@material-ui/core/Button';
import Paper from '@material-ui/core/Paper';
import Typography from '@material-ui/core/Typography';
import { Dialog, DialogTitle, DialogContent, DialogContentText, DialogActions, CircularProgress } from '@material-ui/core';

import grey from '@material-ui/core/colors/grey';

const styles = {
	paper: {
		display: 'flex',
		flexDirection: 'column',
		maxWidth: 400,
		padding: 20,
		margin: 'auto',
		marginTop: '6em'
	},
	button: {
		marginTop: 15
	},
	buttonProgress: {
		color: grey[500]
	}
};

@inject('AuthStore') @observer
class Registration extends Component {
	constructor(props) {
		super(props);

		this.state = {
			isLoading: false,
			isDialogOpen: false,
		}
		this.handleInput = this.handleInput.bind(this);
		this.handleSubmit = this.handleSubmit.bind(this);
		this.handleDialogClose = this.handleDialogClose.bind(this);
	}

	componentWillUnmount() {
		this.props.AuthStore.resetStore();
	}

	handleInput(e) {
		const { AuthStore } = this.props;
		const { name, value } = e.target;
		AuthStore.setFieldValue(name, value);
	}
	
	async handleSubmit(e) {
		e.preventDefault();
		this.setState({ isLoading: true });

		const success = await this.props.AuthStore.register();
		if (success) {
			this.setState({ isDialogOpen: true });
		}
		this.setState({ isLoading: false });
	}

	handleDialogClose(e, reason) {
		this.setState({ isDialogOpen: false });
		this.props.history.push('/auth/login');
	}
	
	render() {
		const { fields, errors } = this.props.AuthStore;
		const { classes } = this.props;
		const { isLoading, isDialogOpen } = this.state;

		return (
			<React.Fragment>

				<Dialog
					open={isDialogOpen}
					onClose={this.handleDialogClose}
					aria-labelledby="alert-dialog-title"
					aria-describedby="alert-dialog-description"
				>
					<DialogTitle id="alert-dialog-title">{"Registration successfull"}</DialogTitle>
					<DialogContent>
						<DialogContentText id="alert-dialog-description">
							Please, check your email and follow the link we've sent to activate your account.
						</DialogContentText>
					</DialogContent>
					<DialogActions>
						<Button onClick={this.handleDialogClose} color="primary" autoFocus>
							ok
						</Button>
					</DialogActions>
				</Dialog>

				<form onSubmit={this.handleSubmit}>
					<Paper className={classes.paper} elevation={1}>
						<Typography variant="h5" gutterBottom>
							Registration
						</Typography>
						<FormControl error={!!errors.fname} margin="dense">
							<InputLabel htmlFor="fname">First Name</InputLabel>
							<Input
								id="fname"
								type="text"
								name="fname"
								value={fields.fname}
								onChange={this.handleInput}
							/>
							<FormHelperText>{errors.fname}</FormHelperText>
						</FormControl>

						<FormControl error={!!errors.lname} margin="dense">
							<InputLabel htmlFor="lname">Last Name</InputLabel>
							<Input
								id="lname"
								type="text"
								name="lname"
								value={fields.lname}
								onChange={this.handleInput}
							/>
							<FormHelperText>{errors.lname}</FormHelperText>
						</FormControl>

						<FormControl error={!!errors.uname} margin="dense">
							<InputLabel htmlFor="uname">Username</InputLabel>
							<Input
								id="uname"
								type="text"
								name="uname"
								value={fields.uname}
								onChange={this.handleInput}
							/>
							<FormHelperText>{errors.uname}</FormHelperText>
						</FormControl>

						<FormControl error={!!errors.email} margin="dense">
							<InputLabel htmlFor="email">Email</InputLabel>
							<Input
								id="email"
								type="email"
								name="email"
								value={fields.email}
								onChange={this.handleInput}
							/>
							<FormHelperText>{errors.email}</FormHelperText>
						</FormControl>

						<FormControl error={!!errors.password} margin="dense">
							<InputLabel htmlFor="password">Password</InputLabel>
							<Input
								id="password"
								type="password"
								name="password"
								value={fields.password}
								onChange={this.handleInput}
							/>
							<FormHelperText>{errors.password}</FormHelperText>
						</FormControl>

						<FormControl error={!!errors.confirmPassword} margin="dense">
							<InputLabel htmlFor="confirmPassword">Confirm password</InputLabel>
							<Input
								id="confirmPassword"
								type="password"
								name="confirmPassword"
								value={fields.confirmPassword}
								onChange={this.handleInput}
							/>
							<FormHelperText>{errors.confirmPassword}</FormHelperText>
						</FormControl>

						<Button
							disabled={isLoading}
							className={classes.button}
							variant="contained"
							size="large"
							color="primary"
							type="submit"
						>
							{isLoading ? <CircularProgress size={24} className={classes.buttonProgress}/> : 'Register'}
						</Button>
					</Paper>
				</form>

			</React.Fragment>
		);
	}
}

export default withStyles(styles)(Registration);