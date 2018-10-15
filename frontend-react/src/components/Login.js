import React, {Component} from 'react';
import "../../node_modules/materialize-social/materialize-social.css";
import { inject, observer } from 'mobx-react';
import FormControl from '@material-ui/core/FormControl';
import FormHelperText from '@material-ui/core/FormHelperText';
import Input from '@material-ui/core/Input';
import InputLabel from '@material-ui/core/InputLabel';
import Grid from '@material-ui/core/Grid';
import Button from '@material-ui/core/Button';
import Typography from '@material-ui/core/Typography';
import intra_logo from "../img/42.png";
import { withStyles } from '@material-ui/core/styles';
import Paper from '@material-ui/core/Paper';
import axios from 'axios';

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
class Login extends Component {
	constructor(props) {
		super(props);
		this.handleInput = this.handleInput.bind(this);
		this.handleSubmit = this.handleSubmit.bind(this);
	}

	handleInput(e) {
		const { AuthStore } = this.props;
		const { name, value } = e.target;
		AuthStore.setFieldValueLogin(name, value);
	}
	
	handleSubmit(e) {
		this.props.AuthStore.login();
	}
	render() {
		const { fields_login, errors_login } = this.props.AuthStore;
		const { classes } = this.props;
		return (
			<Paper className={classes.paper} elevation={1}>
				<Typography component="h5" variant="h5" gutterBottom>
					Log In
				</Typography>

				<FormControl error={!!errors_login.login}>
					<InputLabel htmlFor="login">Login</InputLabel>
					<Input
						id="login"
						type="login"
						name="login"
						value={fields_login.login}
						onChange={this.handleInput}
					/>
					<FormHelperText>{errors_login.login}</FormHelperText>
				</FormControl>

				<FormControl error={!!errors_login.password}>
					<InputLabel htmlFor="password">Password</InputLabel>
					<Input
						id="password"
						type="password"
						name="password"
						value={fields_login.password}
						onChange={this.handleInput}
					/>
					<FormHelperText>{errors_login.password}</FormHelperText>
				</FormControl>

				<Button onClick={this.handleSubmit} variant="contained" color="primary">
					Submit
				</Button>
				  <div className="social">
                    <a href="">
                    	<img src={intra_logo} height="30"/>
                    </a>
                    <a href="">
                        <i className="fa fa-github"></i>
                    </a>
                     <a href="">
                        <i className="fa fa-google-plus-circle " aria-hidden="true"></i>
                    </a>
                </div>
			</Paper>
		);
	}
}


export default withStyles(styles)(Login);