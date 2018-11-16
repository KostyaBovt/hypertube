import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';
import { Typography, Card, ButtonBase, IconButton, CircularProgress, List, ListSubheader, ListItemText, ListItem} from '@material-ui/core';
import not_found from "../img/not_found.jpg";


const styles = theme => ({
	// layout: {
	// 	width: 'auto',
	// 	[theme.breakpoints.up(600 + theme.spacing.unit * 2 * 2)]: {
	// 		width: 600,
	// 		marginLeft: 'auto',
	// 		marginRight: 'auto',
	// 	}
    // },
    layout: {
        marginLeft: 'auto',
        marginRight: 'auto',
        marginTop: theme.spacing.unit * 2,
		marginBottom: theme.spacing.unit * 2,
    },
    container: {
        marginTop: theme.spacing.unit * 2,
        marginBottom: theme.spacing.unit * 2
    },
    item : {
        margin: theme.spacing.unit
    },
    poster: {
        width: "100%",
    }
});

@inject('MovieStore') @observer
class Movie extends Component {
    componentDidMount() {
        this.props.MovieStore.fetchMovie(this.props.match.params.id);
    }

    componentWillUnmount() {
        this.props.MovieStore.resetMovie();
    }

    render() {
        const { classes } = this.props;
        const { movie } = this.props.MovieStore;

        if (movie === undefined) {
            return <CircularProgress />;
        } else if (movie === null) {
            return "ERROR";
        } else {
            return (
                <main className={classes.layout}>
                    <Grid container className={classes.container} justify="center">

                        <Grid item xs={6}>
                            <Paper square>
                                <Grid container>

                                    <Grid item xs={5} className={classes.item}>
                                        <img className={classes.poster} src={movie.poster_path} alt="Movie poster"/>
                                    </Grid>

                                    <Grid item xs className={classes.item}>
                                        <Grid container direction="column">
                                            <Grid item>
                                                <Typography variant="h6">
                                                    {movie.title}
                                                </Typography>
                                            </Grid>
                                            <Grid item>
                                                <Typography gutterBottom variant="subtitle2" color="textSecondary">
                                                    {movie.release_date}
                                                </Typography>
                                            </Grid>
                                            <Grid item>
                                                <Typography gutterBottom variant="subtitle2" color="textSecondary">
                                                    {movie.runtime} minutes
                                                </Typography>
                                            </Grid>
                                            <Grid item>
                                                <Typography variant="subtitle1">
                                                    {movie.overview}
                                                </Typography>
                                            </Grid>
                                        </Grid>
                                    </Grid>

                                </Grid>
                            </Paper>
                        </Grid>

                    </Grid>
                </main>
            );
        }
    }
}

Movie.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(Movie);