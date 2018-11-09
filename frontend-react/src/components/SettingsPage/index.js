import React from 'react';
import { withStyles, Grid } from '@material-ui/core';

import PictureSection from './PictureSection';
import ProfileSection from './ProfileSection';
import AccountSection from './AccountSection';
import OtherSection from './OtherSection';

const styles = theme => ({
	layout: {
		width: 'auto',
		[theme.breakpoints.up(600)]: {
			width: 600,
			marginLeft: 'auto',
			marginRight: 'auto',
		}
	},
	container : {
		display: 'flex',
		flexDirection: 'column',
		marginTop: theme.spacing.unit * 2,
		marginBottom: theme.spacing.unit * 2,
	},
	paper: {
		marginTop: theme.spacing.unit,
		marginBottom: theme.spacing.unit,
		padding: 0
	},
});

class SettingsPage extends React.Component {
    render() {
        const { classes } = this.props;
        return (
            <main className={classes.layout}>
                <Grid className={classes.container} container spacing={0}>
					<Grid item xs={12}>
						<PictureSection />
					</Grid>
					<Grid item xs={12}>
						<ProfileSection />
					</Grid>
					<Grid item xs={12}>
						<AccountSection />
					</Grid>
					<Grid item xs={12}>
						<OtherSection />
					</Grid>
				</Grid>
            </main>
        )
    }
}

export default withStyles(styles)(SettingsPage);