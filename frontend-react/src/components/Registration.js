import React, {Component} from 'react';
import { Link } from 'react-router-dom';
import axios from 'axios';
import { Row, Input } from "react-materialize";

class Registration extends Component {
	render() {
		return (
				<div className="container-reg">
					<Row>
					    <Input  s={12} l={6} label="First Name" />
					    <Input s={12} l={6} label="Last Name" />
					    <Input s={12} l={12} label="User Name" />
					    <Input type="email" label="Email" s={12} />
					    <Input type="password" label="Password" s={12} />
					    <Input type="password"s={12} l={12} label="Repeat Password" />
					    <button className="btn waves-effect waves-light btn-small" type="submit" name="action">Submit
    						<i className="material-icons right">send</i>
  						</button>
					</Row>	
				</div>	
		);
	}
}

export default Registration;