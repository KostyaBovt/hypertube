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
import { Link } from 'react-router-dom';
import { withNamespaces } from 'react-i18next';

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
	},
	forgotPassword: {
		textAlign: 'center'
	},
    forgotPasswordLink: {
        color: 'black',
        textDecoration: 'none'
	},
	intraIco: {
		padding: '10px'
	}
};
@withNamespaces()
@inject('AuthStore') @observer
class Login extends Component {
	constructor(props) {
		super(props);
		this.handleInput = this.handleInput.bind(this);
		this.handleSubmit = this.handleSubmit.bind(this);
	}

	componentWillUnmount() {
		this.props.AuthStore.resetStore();
	}

	handleInput(e) {
		const { AuthStore } = this.props;
		const { name, value } = e.target;
		AuthStore.setFieldValue(name, value);
	}
	
	handleSubmit(e) {
		e.preventDefault();
		this.props.AuthStore.login();
	}

	render() {
		const { fields, errors } = this.props.AuthStore;
		const { classes, t } = this.props;
		return (
			<form onSubmit={this.handleSubmit}>
				<Paper className={classes.paper} elevation={1}>
					<Typography variant="h5" gutterBottom>
                        {t('login:LogIn')}
					</Typography>

					<FormControl error={!!errors.uname} margin="dense">
						<InputLabel htmlFor="uname">{t('login:Username')}</InputLabel>
						<Input
							id="uname"
							type="text"
							name="uname"
							value={fields.uname}
							onChange={this.handleInput}
						/>
						<FormHelperText>{errors.uname}</FormHelperText>
					</FormControl>

					<FormControl error={!!errors.password} margin="dense">
						<InputLabel htmlFor="password">{t('login:Password')}</InputLabel>
						<Input
							id="password"
							type="password"
							name="password"
							value={fields.password}
							onChange={this.handleInput}
						/>
						<FormHelperText>{errors.password}</FormHelperText>
					</FormControl>

					<Button className={classes.button} variant="contained" color="primary" type="submit">
                        {t('login:LogIn')}
					</Button>

					<div className="social">
						<a title="42_Intra" className={classes.intraIco} href="http://localhost:8080/api/auth/intra/login">
							<img src={intra_logo} height="20"/>
						</a>
						<a title="GitHub" href="http://localhost:8080/api/auth/github/login">
							<i className="fa fa-github"></i>
						</a>
						<a title="Google" href="http://localhost:8080/api/auth/google/login">
							<i  className="fa fa-google-plus-circle " aria-hidden="true"></i>
						</a>
					</div>
					<Typography variant="body1" className={classes.forgotPassword}>
						<Link to="/auth/lostpass" className={classes.forgotPasswordLink}>
							{t('login:forgotPassword')}
						</Link>
					</Typography>
				</Paper>
			</form>
		);
	}
}


export default withStyles(styles)(Login);