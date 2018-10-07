import React, { Component } from 'react';
import Logo from './logo.svg';
import './App.css';
import Header from './components/Header';
import Footer from './components/Footer';
import Home from './components/Home';
import Auth from './components/Auth';
import Profile from './components/Profile';
import User from './components/User';
import {Switch, Route, Redirect, withRouter} from 'react-router-dom';


class HyperTube extends Component {
  render() {
    const {storage} = this.props;
    return (
         <React.Fragment>
            <Header storage={storage}/>
            <Switch>

              <Route path="/auth" render={props =>
                storage.isAuthenticated.get() ? (
                  <Redirect to="/"/>
                ) : (
                  <Auth {...props} storage={storage}/>
                )} />

              <PrivateRoute exact path="/" component={Home} storage={storage}/>
              <PrivateRoute exact path="/user/:username" component={User} storage={storage}/>
              <PrivateRoute exact path="/profile" component={Profile} storage={storage}/>
              <Route path="*" render={() => (<h1>Not Found</h1>)}/>
            </Switch>
            <Footer/>
        </React.Fragment>
    );
  }
}

const PrivateRoute =
    ({ component: Component, storage, ...rest }) => {
        return (
            <Route
                {...rest}
                render={props => storage.isAuthenticated ?
                    (<Component {...props} storage={storage}/>) : (<Redirect to="/auth"/>)
                }
            />
        )
    };

export default HyperTube;
