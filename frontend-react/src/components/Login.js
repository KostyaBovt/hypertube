import React, {Component} from 'react';
import { Link } from 'react-router-dom';
import axios from 'axios';
import { Row, Input } from "react-materialize";

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
				</Row>	
			</div>
		);
	}
}

export default Login;