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
class NewPass extends Component {
	constructor(props) {
		super(props);

		this.state = {
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

		const success = await this.props.AuthStore.newPass();
		if (success) {
			this.setState({ isDialogOpen: true });
		}
	}

	handleDialogClose(e, reason) {
		console.log(reason);
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
								Your password has changed, please login.
							</DialogContentText>
						</DialogContent>
						<DialogActions>
							<Button  onClick={this.handleDialogClose} color="primary" autoFocus>
								ok
							</Button>
						</DialogActions>
				</Dialog>

				<Paper className={classes.paper} elevation={1}>
					<Typography variant="h5" gutterBottom>
						Password Recovery
					</Typography>

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

					<Button className={classes.button} onClick={this.handleSubmit} variant="contained" color="primary">
						Submit
					</Button>
				</Paper>

			</React.Fragment>
		);
	}
}

export default withStyles(styles)(NewPass);