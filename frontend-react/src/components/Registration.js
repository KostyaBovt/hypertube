import React, { Component } from 'react';
import { Row, Input, Button, Col } from "react-materialize";
import "../../node_modules/materialize-social/materialize-social.css";
import { inject, observer } from 'mobx-react';

@inject('AuthStore') @observer
class Registration extends Component {
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
		this.props.AuthStore.register();
	}

	render() {
		const { fields, errors } = this.props.AuthStore;

		return (
			<Row>
				<Col s={12} m={6} l={4} className='offset-m3 offset-l4'>
					<div className="container-reg">
						<Row>
							<Input
								label="First Name" 
								type="text"
								name="fname"
								value={fields.fname}
								error={errors.fname}
								onChange={this.handleInput}
								s={12}
							/>
							<Input
								label="Last Name" 
								type="text"
								name="lname"
								value={fields.lname}
								error={errors.lname}
								onChange={this.handleInput}
								s={12}
							/>
							<Input
								label="Username" 
								type="text"
								name="uname"
								value={fields.uname}
								error={errors.uname}
								onChange={this.handleInput}
								s={12}
							/>
							<Input
								label="Email" 
								type="email"
								name="email"
								value={fields.email}
								error={errors.email}
								onChange={this.handleInput}
								s={12}
							/>
							<Input
								label="Password"
								type="password"
								name="password"
								value={fields.password}
								error={errors.password}
								onChange={this.handleInput}
								s={12}
							/>
							<Input
								label="Confirm Password" 
								type="password"
								name="confirmPassword"
								value={fields.confirmPassword}
								error={errors.confirmPassword}
								onChange={this.handleInput}
								s={12}
							/>
							<Button onClick={this.handleSubmit} waves='light'>
								<span>Register</span>
								<i className="material-icons right">send</i>
							</Button>
						</Row>	
					</div>	
				</Col>
			</Row>
		);
	}
}

export default Registration;