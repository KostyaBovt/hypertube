import React, {Component} from 'react';
import "../../node_modules/materialize-social/materialize-social.css";
import { inject, observer } from 'mobx-react';
import FormControl from '@material-ui/core/FormControl';
import FormHelperText from '@material-ui/core/FormHelperText';
import Input from '@material-ui/core/Input';
import InputLabel from '@material-ui/core/InputLabel';
import Button from '@material-ui/core/Button';
import Typography from '@material-ui/core/Typography';
import intra_logo from "../img/42.png";
import { withStyles } from '@material-ui/core/styles';
import Paper from '@material-ui/core/Paper';
import { Dialog, DialogTitle, DialogContent, DialogContentText, DialogActions, CircularProgress } from '@material-ui/core';

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
	authBtnsContainer: {
		display: 'flex',
	}
};


@inject('AuthStore') @observer
class LostPass extends Component {
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

		const success = await this.props.AuthStore.lostPass();
		if (success) {
			this.setState({ isDialogOpen: true });
		}
		this.setState({ isLoading: false });
	}

	handleDialogClose(e, reason) {
		console.log(reason);
		this.setState({ isDialogOpen: false });
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
						<DialogTitle id="alert-dialog-title">{"Recovering email sent"}</DialogTitle>
						<DialogContent>
							<DialogContentText id="alert-dialog-description">
								Please, check your email and follow the link we've sent to recover your password.
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
							Lost Password
						</Typography>

						<FormControl error={!!errors.email} margin="dense">
							<InputLabel htmlFor="email">Your Email</InputLabel>
							<Input
								id="email"
								type="email"
								name="email"
								value={fields.email}
								onChange={this.handleInput}
							/>
							<FormHelperText>{errors.email}</FormHelperText>
						</FormControl>

						<Button 
							disabled={isLoading}
							className={classes.button} 
							variant="contained" 
							color="primary" 
							type="submit"
						>
							{isLoading ? <CircularProgress size={24} className={classes.buttonProgress}/> : 'Submit'}
						</Button>
					</Paper>
				</form>

			</React.Fragment>
		);
	}
}

export default withStyles(styles)(LostPass);