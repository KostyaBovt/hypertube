import React, { Component } from 'react'
import { inject, observer } from 'mobx-react';
import imgHelpers from '../../helpers/imgHelpers';
import { Paper, List, ListItem, ListSubheader, ListItemIcon, Icon, ListItemText, withStyles, Snackbar, IconButton, Avatar } from '@material-ui/core';

const styles = theme => ({
    paper: {
		marginTop: theme.spacing.unit,
		marginBottom: theme.spacing.unit,
		padding: 0
	},
	avatarContainer : {
		display: 'flex',
		justifyContent: 'center'
	},
	avatar: {
		margin: theme.spacing.unit,
		backgroundColor: theme.palette.grey,
		width: 256,
		height: 256
	}
});

@inject('SelfStore') @observer
class PictureSection extends Component {
    constructor(props) {
        super(props);
        this.state = {
            error: '',
            snackbarOpen: false
        }
        this.onFileChange = this.onFileChange.bind(this);
        this.importPictureFromSocial = this.importPictureFromSocial.bind(this);
        this.deleteCurrentPicture = this.deleteCurrentPicture.bind(this);
        this.handleSnackbarClose = this.handleSnackbarClose.bind(this);
		this.fileInput = React.createRef();
    }

    async onFileChange(e) {
		const { SelfStore } = this.props;

		const file = e.target.files[0];
		e.target.value = '';

		if (file && file.size < 3000000) {
			try {
				const src = await imgHelpers.imgFileToBase64(file);
                const result = await SelfStore.updateProfile('avatar', src);
                if (!result.success) {
                    this.setState({
                        snackbarOpen: true,
                        error: result.error
                    });
                }
			} catch (e) {
                this.setState({
                    error: 'image processing failed, please try again',
                    snackbarOpen: true
                });
				console.error(e);
			}
		} else {
            this.setState({
                error: 'file is too big, max size is 3MB',
                snackbarOpen: true
            });
		}
	}
    
    async importPictureFromSocial(e) {
		const { SelfStore } = this.props;
		SelfStore.importPictureFromSocial();
    }
    
    async deleteCurrentPicture(e) {
		const { SelfStore } = this.props;
		await SelfStore.updateProfile('avatar', '');
    }
    
    handleSnackbarClose(e, reason) {
		this.setState({ snackbarOpen: false });
    }
    
    renderSnackBar() {
        const { error, snackbarOpen } = this.state;
		return (
			<Snackbar
				anchorOrigin={{
					vertical: 'bottom',
					horizontal: 'left',
				}}
				open={snackbarOpen}
				autoHideDuration={6000}
				onClose={this.handleSnackbarClose}
				message={<span>Error: {error}</span>}
				action={[
					<IconButton
						key="close"
						aria-label="Close"
						color="inherit"
						onClick={this.handleSnackbarClose}
					>
						<Icon>
							close
						</Icon>
					</IconButton>
				]}
			/>
		)
    }

    renderAvatar(self, classes) {
		if (self.avatar) {
			return <Avatar className={classes.avatar} src={`http://localhost:8080${self.avatar}`} />;
		} else {
			return (
				<Avatar className={classes.avatar} src={self.avatar} >
					{`${self.fname.charAt(0)}${self.lname.charAt(0)}`}
				</Avatar>
			);
		}
    }

    render() {
        const { classes } = this.props;
        const { self } = this.props.SelfStore;
        return (
            <React.Fragment>
                { this.renderSnackBar() }
                <Paper className={classes.paper}>
                    <List disablePadding subheader={ <ListSubheader disableSticky color="primary">Picture</ListSubheader> }>
                        <ListItem divider className={classes.avatarContainer}>
                            { this.renderAvatar(self, classes) }
                        </ListItem>
                        <ListItem divider button onClick={() => { this.fileInput.current.click() }}>
                            <ListItemIcon>
                                <Icon>
                                    cloud_upload
                                </Icon>
                            </ListItemIcon>
                            <ListItemText primary="Upload new picture"/>
                        </ListItem>
                        {
                            self.social_provider &&
                            <ListItem divider button onClick={this.importPictureFromSocial.bind(this)}>
                                <ListItemIcon>
                                    <Icon>
                                        get_app
                                    </Icon>
                                </ListItemIcon>
                                <ListItemText primary={`Import picture from ${self.social_provider}`}/>
                            </ListItem>
                        }
                        <ListItem button onClick={this.deleteCurrentPicture.bind(this)}>
                            <ListItemIcon>
                                <Icon>
                                    delete
                                </Icon>
                            </ListItemIcon>
                            <ListItemText primary="Delete current picture"/>
                        </ListItem>
                    </List>
                    <input
                        style={{ 'display': 'none' }}
                        onChange={this.onFileChange}
                        ref={this.fileInput}
                        type="file"
                        accept="image/*"
                    />
                </Paper>
            </React.Fragment>
        )
    }
}

export default withStyles(styles)(PictureSection);