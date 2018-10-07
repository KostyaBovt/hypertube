import React, {Component} from 'react';
import { Link } from 'react-router-dom';
import axios from 'axios';
import {Switch, Route} from 'react-router-dom';
import Login from './Login';
import Registration from './Registration';
import LostPass from './LostPass';

class Auth extends Component {
	render() {
        const {storage} = this.props;
        return (
            <div className="auth-bg">
            	<Switch>
            		<Route exact path="/auth/login" component={Login} storage={storage}/>
                	<Route exact path="/auth/registration" component={Registration} storage={storage}/>
                 	<Route exact path="/auth/lostpass" component={LostPass} storage={storage}/>
            	</Switch>
            </div>
        )
    }
}

export default Auth;