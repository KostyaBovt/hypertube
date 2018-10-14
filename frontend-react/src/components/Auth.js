import React, {Component} from 'react';
import { Redirect } from 'react-router-dom';
import {Switch, Route} from 'react-router-dom';
import Login from './Login';
import Registration from './Registration';
import LostPass from './LostPass';
import { inject, observer } from 'mobx-react';

@inject('UserStore') @observer
class Auth extends Component {
	render() {
        const { UserStore } = this.props;
        console.log('auth');
        if (UserStore.self) {
            return <Redirect to="/" />;
        } else {
            return (
                <div className="auth-bg">
                    <Switch>
                        <Route exact path="/auth/login" component={Login}/>
                        <Route exact path="/auth/registration" component={Registration}/>
                        <Route exact path="/auth/lostpass" component={LostPass}/>
                    </Switch>
                </div>
            )
        }
    }
}

export default Auth;