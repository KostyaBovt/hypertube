import React, { Component } from 'react';
import { withStyles } from '@material-ui/core/styles';
import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import Button from '@material-ui/core/Button';
import { inject, observer } from 'mobx-react';
import { IconButton, Icon, Menu, MenuItem } from '@material-ui/core';

import { Link } from 'react-router-dom'

const styles = {
    root: {
        flexGrow: 1,
    },
    grow: {
        flexGrow: 1,
    },
    buttons: {
        marginLeft: 5
    }
};

@inject('UserStore', 'AuthStore') @observer
class Header extends Component {
  constructor(props) {
    super(props);
    this.logoutUser = this.logoutUser.bind(this);
  }

  logoutUser() {
    this.props.UserStore.forgetSelf();
    this.props.AuthStore.logout();
  }

  renderAuthButtons(classes) {
    if (this.props.UserStore.self) {
        return (
            <React.Fragment>
                <Button component={Link} to="/profile" className={classes.buttons} color="inherit">Profile</Button>
                <Button onClick={this.logoutUser} className={classes.buttons} color="inherit">Logout</Button>
            </React.Fragment>
        )
    } else {
        return (
            <React.Fragment>
                <Button component={Link} to="/auth/registration" className={classes.buttons} color="inherit">Register</Button>
                <Button component={Link} to="/auth/login" className={classes.buttons} color="inherit">Login</Button>
            </React.Fragment>
        )
    }
  }
  
  render() {
        const { classes } = this.props;
        return (
            <div className={classes.root}>
                <AppBar position="static">
                    <Toolbar>
                        <Typography id="logo" variant="h6" color="inherit" className={classes.grow}>
                            HyperTube
                        </Typography>
                        { this.renderAuthButtons(classes) }
                    </Toolbar>
                </AppBar>
            </div>
        );
	}
};


export default withStyles(styles)(Header);
