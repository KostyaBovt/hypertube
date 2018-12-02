import React, { Component } from 'react'
import { Paper, withStyles, List, ListItem, ListItemText, ListSubheader, Dialog, DialogTitle, DialogContent, RadioGroup, FormControlLabel, Radio, DialogActions, Button, CircularProgress } from '@material-ui/core';
import { inject, observer } from 'mobx-react';
import { withNamespaces } from 'react-i18next';

const styles = theme => ({
	paper: {
		marginTop: theme.spacing.unit,
		marginBottom: theme.spacing.unit,
		padding: 0
    }
});

@withNamespaces()
@inject('SelfStore') @observer
class OtherSection extends Component {
    constructor(props) {
        super(props);
        this.state = {
            isLoading: false,
            dialogOpen: false,
            value: ''
        };

        this.openDialog = this.openDialog.bind(this);
        this.handleDialogClose = this.handleDialogClose.bind(this);
        this.updateLocale = this.updateLocale.bind(this);
    }

    handleFormSubmit(e) {
        e.preventDefault();
    }

    handleDialogClose(e, reason) {
        this.setState({ dialogOpen: false });
    }

    openDialog(e) {
        const { self } = this.props.SelfStore;
        this.setState({
            dialogOpen: true,
            value: self.locale
        });
    }

    async updateLocale() {
        const locale = this.state.value;
        const { SelfStore } = this.props;

        if (locale === SelfStore.self.locale) {
            return;
        } else {
            this.setState({ isLoading: true });
            await SelfStore.updateLocale(locale);
            this.setState({ isLoading: false });
            this.handleDialogClose();
        }
    }

    renderDialog(t) {
        const { dialogOpen, value, isLoading } = this.state;
		return (
			<Dialog
				open={dialogOpen}
				onClose={this.handleDialogClose}
                aria-labelledby="locale-dialog-title"
                maxWidth="xs"
			>
				<DialogTitle id="locale-dialog-title">{t('settingsPage:language')}</DialogTitle>
				<DialogContent>
					<form onSubmit={this.handleFormSubmit.bind(this)}>
                        <RadioGroup
                            aria-label="Language"
                            name="locale"
                            value={value}
                            onChange={(e, value) => this.setState({ value })}
                        >
                            <FormControlLabel value="en" control={<Radio />} label="English" />
                            <FormControlLabel value="ru" control={<Radio />} label="Русский" />
                        </RadioGroup>
					</form>
				</DialogContent>
				<DialogActions>
					<Button onClick={this.handleDialogClose} color="primary">
                        {t('settingsPage:cancel')}
					</Button>
					<Button disabled={isLoading} onClick={this.updateLocale} color="primary">
						{isLoading ? <CircularProgress size={18}/> : t('settingsPage:save')}
					</Button>
				</DialogActions>
			</Dialog>
		);
	}
    
    render() {
        const { classes, t } = this.props;
        const { self } = this.props.SelfStore;
        const selectedLanguage = self.locale === 'en' ? 'English' : 'Русский';

        return (
            <React.Fragment>
                { this.renderDialog(t) }
                <Paper className={classes.paper}>
                    <List disablePadding subheader={
                        <ListSubheader disableSticky color="primary">{t('settingsPage:other')}</ListSubheader>
                    }>
                        <ListItem id="locale" button onClick={this.openDialog}>
                            <ListItemText
                                primary={selectedLanguage}
                                secondary={t('settingsPage:language')}
                            />
                        </ListItem>
                    </List>
                </Paper>
            </React.Fragment>
        )
    }
}

export default withStyles(styles)(OtherSection);
