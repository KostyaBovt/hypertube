import React, { Component } from 'react';
import "../../node_modules/materialize-social/materialize-social.css";
import { inject, observer } from 'mobx-react';
import FormControl from '@material-ui/core/FormControl';
import FormHelperText from '@material-ui/core/FormHelperText';
import Input from '@material-ui/core/Input';
import InputLabel from '@material-ui/core/InputLabel';
import Grid from '@material-ui/core/Grid';
import Button from '@material-ui/core/Button';

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
		return (
			<Grid container spacing={16}>
				<Grid item xs={12}>
					<Grid container justify="center" spacing={16}>
						<div className="container-reg">
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
								<InputLabel htmlFor="confirmPassword">Password</InputLabel>
								<Input
									id="confirmPassword"
									type="password"
									name="confirmPassword"
									value={fields.confirmPassword}
									onChange={this.handleInput}
								/>
								<FormHelperText>{errors.confirmPassword}</FormHelperText>
							</FormControl>

							<Button onClick={this.handleSubmit} variant="contained" color="primary">
								Primary
							</Button>
						</div>	
					</Grid>
				</Grid>
			</Grid>
		);
	}
}

export default Registration;