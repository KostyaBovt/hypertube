import React, {Component} from 'react';
import { Link } from 'react-router-dom';
import axios from 'axios';
import { Row, Input } from "react-materialize";

class Login extends Component {
	render() {
		return (
			<div className="container">
				<Row>
				 	<Input type="email" label="User Name" s={12} />
				    <Input type="password" label="Password" s={12} />
				    <Input type="password" label="Repeat Password" s={12} />
				</Row>	
			</div>
		);
	}
}

export default Login;