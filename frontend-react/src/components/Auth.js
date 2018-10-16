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
        const { UserStore, match } = this.props;
        if (UserStore.self) {
            return <Redirect to="/"/>;
        } else {
            return (
                <Switch>
                    <Route path={`${match.path}/login`} component={Login}/>
                    <Route path={`${match.path}/registration`} component={Registration}/>
                    <Route path={`${match.path}/lostpass`} component={LostPass}/>
                    <Redirect to="/auth/login"/>
                </Switch>
            );
        }
    }
}

export default Auth;