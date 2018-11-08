import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';


const styles = theme => ({
	layout: {
		width: 'auto',
		[theme.breakpoints.up(600 + theme.spacing.unit * 2 * 2)]: {
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
	}
});

@inject('MovieStore') @observer
class Movie extends Component {
    componentDidMount() {
        this.props.MovieStore.fetchMovie(458156);
    }

    render() {
        const { classes } = this.props;

        return (
            <main className={classes.layout}>
                <Grid container spacing={8}>
                    <Grid item xs={12}>
                        <Paper>
                            Hello!
                        </Paper>
                    </Grid>
                    <Grid item xs={12}>
                        <Paper>
                            Hello!
                        </Paper>
                    </Grid>
                    <Grid item xs={12}>
                        <Paper>
                            Hello!
                        </Paper>
                    </Grid>
                </Grid>
            </main>
        );
    }
}

Movie.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(Movie);