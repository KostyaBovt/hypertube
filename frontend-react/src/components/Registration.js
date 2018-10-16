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

const styles = {
	paper: {
		display: 'flex',
		flexDirection: 'column',
		maxWidth: 400,
		padding: 20,
		margin: 'auto',
		marginTop: '6em'
	}
  };

@inject('AuthStore') @observer
class Registration extends Component {
	constructor(props) {
		super(props);
		this.handleInput = this.handleInput.bind(this);
		this.handleSubmit = this.handleSubmit.bind(this);
	}

	handleInput(e) {
		const { AuthStore } = this.props;
		const { name, value } = e.target;
		AuthStore.setFieldValue(name, value);
	}
	
	handleSubmit(e) {
		this.props.AuthStore.register();
	}
	
	render() {
		const { fields, errors } = this.props.AuthStore;
		const { classes } = this.props;
		return (
			<Paper className={classes.paper} elevation={1}>
				<Typography variant="h5" gutterBottom>
					Registration
				</Typography>
				<FormControl error={!!errors.fname}>
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

				<FormControl error={!!errors.lname}>
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

				<FormControl error={!!errors.uname}>
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

				<FormControl error={!!errors.email}>
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

				<FormControl error={!!errors.password}>
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

				<FormControl error={!!errors.confirmPassword}>
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

				<Button onClick={this.handleSubmit} variant="contained" size="large" color="primary">
					Register
				</Button>
			</Paper>
		);
	}
}

export default withStyles(styles)(Registration);