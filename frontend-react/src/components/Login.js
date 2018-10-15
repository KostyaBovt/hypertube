import React, {Component} from 'react';
import { Link } from 'react-router-dom';
import axios from 'axios';
import { Row, Input, Button, Col } from "react-materialize";
import "../../node_modules/materialize-social/materialize-social.css";
import intra_logo from "../img/42.png";

class Login extends Component {
	render() {
		console.log('rendering login');
		return (
			<div className="container-log">
				<Row>
				 	<Input type="email" label="Email or User Name" s={12} m={12} l={12} xl={12}/>
				    <Input type="password" label="Password" s={12} m={12} l={12} xl={12}/>
				    	<button className="btn waves-effect waves-light btn-small" type="submit" name="action">Submit
    					<i className="material-icons right">send</i>
  					</button>
  					<div className="social">

  					<Col s={3} m={3} l={3} xl={3} >
						<a className="waves-effect waves-red btn-floating social google">
							<i className="fab fa-google"></i> Sign in with google
						</a>
						</Col>
						<Col s={3} m={3} l={3} xl={3} >
						<a className="waves-effect waves-light btn-floating ">
						<img src={intra_logo} alt="42_intra" width="40" height="40"/>
						</a>
						</Col>
						<Col s={3} m={3} l={3} xl={3} >
						<a className="waves-effect waves-light btn-floating social github">
							<i className="fab fa-github"></i> Sign in with github
						</a>
						</Col>
					</div>
				</Row>	
			</div>
		);
	}
}

export default Login;