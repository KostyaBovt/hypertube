import React, { Component } from 'react'
import { inject, observer } from 'mobx-react';
import { Paper, List, ListItem, ListSubheader, ListItemText, Dialog, DialogTitle, DialogContent, FormControl, InputLabel, Input, FormHelperText, DialogActions, Button, CircularProgress, withStyles } from '@material-ui/core';
import simpleValidator from '../../helpers/simpleValidator';
import { withNamespaces } from 'react-i18next';

const styles = theme => ({
    paper: {
		marginTop: theme.spacing.unit,
		marginBottom: theme.spacing.unit,
		padding: 0
	},
});


@withNamespaces()
@inject('SelfStore') @observer
class ProfileSection extends Component {
    constructor(props) {
        super(props);
        this.state = {
            isLoading: false,
			isDialogOpen: false,
			selectedField: "fname",
			inputValue: '',
			inputError: '',
        };
        this.handleItemClick = this.handleItemClick.bind(this);
        this.handleInput = this.handleInput.bind(this);
        this.handleFormSubmit = this.handleFormSubmit.bind(this);
        this.saveFieldValue = this.saveFieldValue.bind(this);
        this.handleDialogClose = this.handleDialogClose.bind(this);
    }

    handleFormSubmit(e) {
        e.preventDefault();
        this.saveFieldValue();
    }

    handleItemClick(e) {
		const { self } = this.props.SelfStore;
		const selectedField = e.currentTarget.id;

		this.setState({
			selectedField,
            isDialogOpen: true,
			inputValue: self[selectedField],
            inputError: ''
		});
    }

    handleInput(e) {
		this.setState({
			inputValue: e.target.value,
			inputError: ''
		});
    }
    

    handleDialogClose(e, reason) {
        this.setState({ isDialogOpen: false });
    }
    
    isValidField(fieldName, value) {
        const result = simpleValidator(fieldName, value);
        if (result.isValid) {
            return true;
        } else {
            this.setState({ inputError: result.error });
            return false;
        }
    }

    async saveFieldValue(e) {
		const { self } = this.props.SelfStore;
		let { selectedField, inputValue } = this.state;

		if (inputValue === null) {
			return;
		}

		inputValue = inputValue.trim();
		if (inputValue === self[selectedField]) {
			this.setState({ isDialogOpen: false });
			return;
        }
        
        if (this.isValidField(selectedField, inputValue)) {
            this.setState({ isLoading: true });
            const result = await this.props.SelfStore.updateProfile(selectedField, inputValue);
            console.log(result);
            if (result.success) {
                this.setState({
                    isLoading: false,
                    isDialogOpen: false
                });
            } else {
                this.setState({
                    isLoading: false,
                    inputError: result.error
                });
            }
        }
	}
    
    renderInputDialog(t) {
        const { isLoading, isDialogOpen, selectedField, inputError, inputValue } = this.state;
        return (
			<Dialog
				open={isDialogOpen}
				onClose={this.handleDialogClose}
				aria-labelledby="form-dialog-title"
				fullWidth
			>
				<DialogTitle id="form-dialog-title">
                    {t('settingsPage:changeField')} {t('settingsPage:' + selectedField).toLowerCase()}
                    </DialogTitle>
				<DialogContent>
					<form onSubmit={this.handleFormSubmit.bind(this)}>
                        <FormControl error={!!inputError} fullWidth>
                            <InputLabel htmlFor={selectedField}>{t('settingsPage:' + selectedField)}</InputLabel>
                            <Input
                                id={selectedField}
                                type="text"
                                name={selectedField}
                                value={inputValue || ''}
                                autoFocus
                                onChange={this.handleInput}
                                multiline={selectedField === 'bio'}
                            />
                            <FormHelperText>{inputError}</FormHelperText>
                        </FormControl>
					</form>
				</DialogContent>
				<DialogActions>
					<Button onClick={this.handleDialogClose} color="primary">
                        {t('settingsPage:cancel')}
					</Button>
					<Button disabled={isLoading} onClick={this.saveFieldValue} color="primary">
						{isLoading ? <CircularProgress size={18}/> : t('settingsPage:save')}
					</Button>
				</DialogActions>
			</Dialog>
		);
    }

    render() {
        const { classes, t } = this.props;
        const { self } = this.props.SelfStore;
        return (
            <React.Fragment>
                { this.renderInputDialog(t) }
                <Paper className={classes.paper}>
                    <List disablePadding subheader={
                        <ListSubheader disableSticky color="primary">{t('settingsPage:profile')}</ListSubheader>
                    }>
                        <ListItem id="fname" button divider onClick={this.handleItemClick}>
                            <ListItemText
                                primary={self.fname || t('settingsPage:none')}
                                secondary={t('settingsPage:fname')}
                            />
                        </ListItem>
                        <ListItem id="lname" button divider onClick={this.handleItemClick}>
                            <ListItemText
                                primary={self.lname || t('settingsPage:none')}
                                secondary={t('settingsPage:lname')}
                            />
                        </ListItem>
                        <ListItem id="uname" button divider onClick={this.handleItemClick}>
                            <ListItemText
                                primary={self.uname}
                                secondary={t('settingsPage:uname')}
                            />
                        </ListItem>
                        <ListItem id="bio" button onClick={this.handleItemClick}>
                            <ListItemText
                                primary={self.bio || t('settingsPage:none')}
                                secondary={t('settingsPage:bio')}
                            />
                        </ListItem>
                    </List>
                </Paper>
            </React.Fragment>
        )
    }
}

export default withStyles(styles)(ProfileSection);
