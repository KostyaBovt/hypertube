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
class LostPass extends Component {
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
		e.preventDefault();
		this.props.AuthStore.lostPass();
	}

	render() {
		const { fields, errors } = this.props.AuthStore;
		const { classes } = this.props;
		return (
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

					<Button className={classes.button} variant="contained" color="primary" type="submit">
						Submit
					</Button>
				</Paper>
			</form>
		);
	}
}

export default withStyles(styles)(LostPass);