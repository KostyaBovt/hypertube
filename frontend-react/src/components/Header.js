import React, { Component } from 'react';
import { withStyles } from '@material-ui/core/styles';
import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import Button from '@material-ui/core/Button';
import { inject, observer } from 'mobx-react';
import imgHelpers from '../helpers/imgHelpers';
import { withNamespaces } from 'react-i18next';

import { Link } from 'react-router-dom'
import { IconButton, Menu, MenuItem, ListItemIcon, Icon } from '@material-ui/core';

const styles = {
    root: {
        flexGrow: 1,
    },
    grow: {
        flexGrow: 1,
    },
    buttons: {
        marginLeft: 5
    },
};

@withNamespaces()
@inject('SelfStore', 'AuthStore') @observer
class Header extends Component {
  constructor(props) {
    super(props);
    this.state = {
        anchorEl: null
    };

    this.logoutUser = this.logoutUser.bind(this);
    this.handleMenu = this.handleMenu.bind(this);
    this.handleMenuClose = this.handleMenuClose.bind(this);
  }

  logoutUser() {
    this.handleMenuClose();
    this.props.SelfStore.forgetSelf();
    this.props.AuthStore.logout();
  }

  handleMenu(e) {
    this.setState({ anchorEl: e.currentTarget });
  }

  handleMenuClose(e) {
    this.setState({ anchorEl: null });
  }

  renderAuthButtons(classes, t) {
    const { self } = this.props.SelfStore;
    const { anchorEl } = this.state;

    if (self) {
        const isMenuOpen = !!anchorEl;

        return (
            <React.Fragment>
                <IconButton
                  aria-owns={isMenuOpen ? 'menu-appbar' : undefined}
                  aria-haspopup="true"
                  onClick={this.handleMenu}
                >
                  { imgHelpers.renderAvatar(self) }
                </IconButton>
                <Menu
                  id="menu-appbar"
                  anchorEl={anchorEl}
                  anchorOrigin={{
                    vertical: 'top',
                    horizontal: 'right',
                  }}
                  transformOrigin={{
                    vertical: 'top',
                    horizontal: 'right',
                  }}
                  open={isMenuOpen}
                  onClose={this.handleMenuClose}
                >
                    <MenuItem onClick={this.handleMenuClose} button component={Link} to="/settings" >
                        <ListItemIcon className={classes.icon}>
                            <Icon>settings</Icon>
                        </ListItemIcon>
                        <Typography variant="inherit" noWrap>
                            {t('header:settings')}
                        </Typography>
                    </MenuItem>
                    <MenuItem button onClick={this.logoutUser} >
                        <ListItemIcon className={classes.icon}>
                            <Icon>exit_to_app</Icon>
                        </ListItemIcon>
                        <Typography variant="inherit" noWrap>
                            {t('header:logout')}
                        </Typography>
                    </MenuItem>
                </Menu>
            </React.Fragment>
        )
    } else {
        return (
            <React.Fragment>
                <Button component={Link} to="/auth/registration" className={classes.buttons} color="inherit">
                    {t('header:register')}
                </Button>
                <Button component={Link} to="/auth/login" className={classes.buttons} color="inherit">
                    {t('header:login')}
                </Button>
            </React.Fragment>
        )
    }
  }
  
  render() {
        const { classes, t } = this.props;
        return (
            <div className={classes.root}>
                <AppBar position="static">
                    <Toolbar>
                        <Typography id="logo" variant="h6" color="inherit" className={classes.grow}>
                            HyperTube
                        </Typography>
                        { this.renderAuthButtons(classes, t) }
                    </Toolbar>
                </AppBar>
            </div>
        );
	}
};


export default withStyles(styles)(Header);
