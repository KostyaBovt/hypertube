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
class RecoverPass extends Component {
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
		this.props.AuthStore.recoverPass();
	}
	
	render() {
		const { fields, errors } = this.props.AuthStore;
		const { classes } = this.props;
		return (
			<Paper className={classes.paper} elevation={1}>
				<Typography variant="h5" gutterBottom>
					Password Recovery
				</Typography>

				<FormControl error={!!errors.uname} margin="dense">
					<InputLabel htmlFor="uname">Password</InputLabel>
					<Input
						id="password"
						type="password"
						name="password"
						value={fields.password}
						onChange={this.handleInput}
					/>
					<FormHelperText>{errors.password}</FormHelperText>
				</FormControl>

				<FormControl error={!!errors.password} margin="dense">
					<InputLabel htmlFor="password">Repeat Password</InputLabel>
					<Input
						id="password"
						type="password"
						name="password"
						value={fields.confirmPassword}
						onChange={this.handleInput}
					/>
					<FormHelperText>{errors.password}</FormHelperText>
				</FormControl>

				<Button className={classes.button} onClick={this.handleSubmit} variant="contained" color="primary">
					Submit
				</Button>
			</Paper>
		);
	}
}

export default withStyles(styles)(RecoverPass);