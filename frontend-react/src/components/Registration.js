import React, {Component} from 'react';
import { Link } from 'react-router-dom';
import axios from 'axios';
import { Row, Input, Button } from "react-materialize";
import "../../node_modules/materialize-social/materialize-social.css";

class Registration extends Component {
	render() {
		return (
				<div className="container-reg">
					<Row>
					    <Input  s={12} l={6} name="name" label="First Name" />
					    <Input s={12} l={6} label="Last Name" />
					    <Input s={12} l={12} label="User Name" />
					    <Input type="email" label="Email" s={12} />
					    <Input type="password" label="Password" s={12} />
					    <Input type="password"s={12} l={12} label="Repeat Password" />
						<Button waves='light' >
							<span>Submit</span>
							<i className="material-icons right">send</i>
						</Button>
						{/* <a class="waves-effect waves-light btn-floating social google">
							<i class="fab fa-google"></i> Sign in with google
						</a> */}
					</Row>	
				</div>	
		);
	}
}

export default Registration;