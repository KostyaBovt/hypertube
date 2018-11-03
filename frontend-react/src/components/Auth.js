import React, {Component} from 'react';
import { Redirect } from 'react-router-dom';
import {Switch, Route} from 'react-router-dom';
import Login from './Login';
import Registration from './Registration';
import LostPass from './LostPass';
import RecoverPass from './RecoverPass';
import { inject, observer } from 'mobx-react';

@inject('SelfStore') @observer
class Auth extends Component {
	render() {
        const { SelfStore, match } = this.props;
        console.log("us: ", SelfStore.self);
        if (SelfStore.self) {
            return <Redirect to="/"/>;
        } else {
            return (
                <Switch>
                    <Route path={`${match.path}/login`} component={Login}/>
                    <Route path={`${match.path}/registration`} component={Registration}/>
                    <Route path={`${match.path}/lostpass`} component={LostPass}/>
                    <Route path={`${match.path}/recoverpass`} component={RecoverPass}/>
                    <Redirect to="/auth/login"/>
                </Switch>
            );
        }
    }
}

export default Auth;