import React, {Component} from 'react';
import { Link } from 'react-router-dom';
import axios from 'axios';
import { Row, Input } from "react-materialize";

class LostPass extends Component {
	render() {
		return (
			<div className="container-log">
			<Row>
				<Input type="password" label="New Password" s={12} />
				<Input type="password" label="Repeat Password" s={12} />
				 <button className="btn waves-effect waves-light btn-small" type="submit" name="action">Submit
    				<i className="material-icons right">send</i>
  				</button>
				</Row>		
			</div>
		);
	}
}

export default LostPass;