import React, { Component } from 'react'
import { inject, observer } from 'mobx-react';
import { Paper, withStyles, List, ListItem, ListItemText, ListSubheader, Dialog, DialogTitle, DialogContent, FormControl, InputLabel, Input, FormHelperText, DialogActions, Button, CircularProgress, DialogContentText } from '@material-ui/core';
import { validate } from 'email-validator';

const styles = theme => ({
	paper: {
		marginTop: theme.spacing.unit,
		marginBottom: theme.spacing.unit,
		padding: 0
    }
});

@inject('SelfStore') @observer
class AccountSection extends Component {
    constructor(props) {
        super(props);
        this.state = {
            isLoading: false,
            emailDialogOpen: false,
            passwordDialogOpen: false,
            email: '',
            old_password: '',
            new_password: '',
            confirm: '',
            errors: {}
        }
        this.handleItemClick = this.handleItemClick.bind(this);
        this.handleInput = this.handleInput.bind(this);
        this.closeDialogs = this.closeDialogs.bind(this);
        this.handleFormSubmit = this.handleFormSubmit.bind(this);
        this.updateEmail = this.updateEmail.bind(this);
        this.updatePassword = this.updatePassword.bind(this);
    }

    handleFormSubmit(e) {
        e.preventDefault();
        const { emailDialogOpen, passwordDialogOpen } = this.state;
        
        if (emailDialogOpen) {
            this.updateEmail();
        } else if (passwordDialogOpen) {
            this.updatePassword();
        }
    }

    handleItemClick(e) {
        const { id } = e.currentTarget;
        if (id === 'email') {
            const { self } = this.props.SelfStore;
            this.setState({
                emailDialogOpen: true,
                email: self.email || '',
                errors: {}
            });
        } else if (id === 'password') {
            this.setState({
                passwordDialogOpen: true, 
                errors: {}
            });
        }
    }

    closeDialogs(e) {
        this.setState({
            emailDialogOpen: false,
            passwordDialogOpen: false
        });
    }

    handleInput(e) {
        const { value, name } = e.target;
        this.setState({ [name]: value, errors: { [name]: '' } });
    }

    async updateEmail() {
        const { email } = this.state;
        const emailIsValid = validate(email);
        if (emailIsValid) {
            this.setState({ isLoading: true });
            const { SelfStore } = this.props;
            const result = await SelfStore.updateEmail(email);
            if (result.success) {
                this.closeDialogs();
            } else {
                this.setState({ errors: { email: result.error }});
            }
            this.setState({ isLoading: false });
        } else {
            this.setState({ errors: { email: 'Email is invalid' }});
        }
    }

    async updatePassword() {
        const { old_password, new_password, confirm } = this.state
        
        if (new_password !== confirm) {
            return this.setState({ errors: { confirm: "Passwords doesn't match" } });
        }

        this.setState({ isLoading: true });
        const result = await this.props.SelfStore.updatePassword(old_password, new_password);
        this.setState({ isLoading: false });
        if (result.success) {
            this.closeDialogs();
            this.setState({
                old_password: '',
                new_password: '',
                confirm: '',
            });
        } else {
            this.setState({ errors: result.error });
        }
    }

    renderPasswordDialog() {
        const { isLoading, passwordDialogOpen, old_password, new_password, confirm, errors } = this.state;
        return (
            <Dialog
				open={passwordDialogOpen}
				onClose={this.closeDialogs}
				aria-labelledby="password-dialog-title"
				fullWidth
			>
				<DialogTitle id="password-dialog-title">Change password</DialogTitle>
				<DialogContent>
					<form onSubmit={this.handleFormSubmit}>
                        <FormControl error={!!errors.old_password} margin="dense" required fullWidth>
                            <InputLabel htmlFor="old_password">Current password</InputLabel>
                            <Input
                                type="password"
                                name="old_password"
                                value={old_password}
                                autoFocus
                                onChange={this.handleInput}
                            />
                            <FormHelperText>{errors.old_password}</FormHelperText>
                        </FormControl>
                        <FormControl error={!!errors.new_password} margin="dense" required fullWidth>
                            <InputLabel htmlFor="new_password">New password</InputLabel>
                            <Input
                                type="password"
                                name="new_password"
                                value={new_password}
                                onChange={this.handleInput}
                            />
                            <FormHelperText>{errors.new_password}</FormHelperText>
                        </FormControl>
                        <FormControl error={!!errors.confirm} margin="dense" required fullWidth>
                            <InputLabel htmlFor="confirm">Confirm new password</InputLabel>
                            <Input
                                type="password"
                                name="confirm"
                                value={confirm}
                                onChange={this.handleInput}
                            />
                            <FormHelperText>{errors.confirm}</FormHelperText>
                        </FormControl>
					</form>
				</DialogContent>
				<DialogActions>
					<Button onClick={this.closeDialogs} color="primary">
						Cancel
					</Button>
					<Button disabled={isLoading} onClick={this.updatePassword} color="primary">
						{isLoading ? <CircularProgress size={18}/> : 'Save'}
					</Button>
				</DialogActions>
			</Dialog>
        );
    }

    renderEmailDialog() {
        const { isLoading, emailDialogOpen, email, errors } = this.state;
        return (
            <Dialog
				open={emailDialogOpen}
				onClose={this.closeDialogs}
				aria-labelledby="email-dialog-title"
				fullWidth
			>
				<DialogTitle id="email-dialog-title">Email address</DialogTitle>
				<DialogContent>
                    <DialogContentText>
                        New email address will be saved only if you confirmed it by following our confirmation link.
                    </DialogContentText>
					<form onSubmit={this.handleFormSubmit}>
                        <FormControl error={!!errors.email} margin="normal" required fullWidth>
                            <InputLabel htmlFor="email">Email</InputLabel>
                            <Input
                                id="email"
                                type="email"
                                name="email"
                                value={email}
                                autoFocus
                                onChange={this.handleInput}
                            />
                            <FormHelperText>{errors.email}</FormHelperText>
                        </FormControl>
					</form>
				</DialogContent>
				<DialogActions>
					<Button onClick={this.closeDialogs} color="primary">
						Cancel
					</Button>
					<Button disabled={isLoading} onClick={this.updateEmail} color="primary">
						{isLoading ? <CircularProgress size={18}/> : 'ok'}
					</Button>
				</DialogActions>
			</Dialog>
        );
    }

    render() {
        const { classes } = this.props;
        const { self } = this.props.SelfStore;
        return (
            <React.Fragment>
                { this.renderEmailDialog() }
                { this.renderPasswordDialog() }
                <Paper className={classes.paper}>
                    <List disablePadding subheader={ <ListSubheader disableSticky color="primary">Account</ListSubheader> }>
                        {
                            !self.social_provider &&
                            <ListItem id="password" button divider onClick={this.handleItemClick}>
                                <ListItemText
                                    primary={"Change password"}
                                />
                            </ListItem>
                        }
                        <ListItem id="email" button onClick={this.handleItemClick}>
                            <ListItemText
                                primary={"Change email"}
                            />
                        </ListItem>
                    </List>
                </Paper>
            </React.Fragment>
        )
    }
}

export default withStyles(styles)(AccountSection);
