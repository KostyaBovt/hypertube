import React, { Component } from 'react';
import './HyperTube.css';
import Header from './components/Header';
import Footer from './components/Footer';
import Home from './components/Home';
import Film from './components/Film';
import Auth from './components/Auth';
import NotFound from './components/NotFound';
import User from './components/User';
import {Switch, Route, Redirect, BrowserRouter} from 'react-router-dom';
import { inject, observer } from 'mobx-react';
import CssBaseline from '@material-ui/core/CssBaseline';
import { MuiThemeProvider, createMuiTheme } from '@material-ui/core/styles';
import teal from '@material-ui/core/colors/teal';
import SettingsPage from './components/SettingsPage';

const theme = createMuiTheme({
  palette: {
    primary: teal
  },
  typography: {
    useNextVariants: true,
  },
});

@inject('SelfStore') @observer
class HyperTube extends Component {
    componentDidMount(){
        const { SelfStore } = this.props;
        
        SelfStore.pullSelf();
        console.log("us: ", SelfStore.self);
    }
  render() {
      const { SelfStore } = this.props;
      if (SelfStore.self === undefined) {
          return null;
      } else {
          return (
              <MuiThemeProvider theme={theme}>
                  <BrowserRouter>
                      <React.Fragment>
                          <CssBaseline/>
                          <Header/>
                          <Switch>
                              <Route path="/auth" component={Auth}/>
                              <PrivateRoute exact path="/" component={Home}/>
                              <PrivateRoute exact path="/user/:username" component={User}/>
                              <PrivateRoute exact path="/settings" component={SettingsPage}/>
                              <PrivateRoute exact path="/film/:id" component={Film}/>
                              <Route path="*" component={NotFound}/>
                          </Switch>
                          <Footer/>
                      </React.Fragment>
                  </BrowserRouter>
              </MuiThemeProvider>
          );
      }
  }
}

@inject('SelfStore') @observer
class PrivateRoute extends Component {
  render() {
    const { SelfStore, component: Component, ...rest } = this.props;
    return (
			<Route {...rest} render={(props) => (
				SelfStore.self
					? <Component {...props} />
					: <Redirect to='/auth/login' />
			)} />
		);
  }
}

export default HyperTube;
