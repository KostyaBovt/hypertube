import React, {Component} from 'react';
import "../../node_modules/materialize-social/materialize-social.css";
import { inject, observer } from 'mobx-react';
import FormControl from '@material-ui/core/FormControl';
import FormHelperText from '@material-ui/core/FormHelperText';
import Input from '@material-ui/core/Input';
import InputLabel from '@material-ui/core/InputLabel';
import Grid from '@material-ui/core/Grid';
import Button from '@material-ui/core/Button';
import intra_logo from "../img/42.png";

@inject('AuthStore') @observer
class Login extends Component {
	render() {
		console.log('rendering login');
		return (
			<div className="container-log">
			</div>
		);
	}
}

export default Login;