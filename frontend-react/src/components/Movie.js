import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';
import { Typography, CircularProgress, FilledInput, InputLabel, Icon, Button, FormControl} from '@material-ui/core';
import ReactPlayer from 'react-player';
import Plyr from 'react-plyr';



const styles = theme => ({
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
    media: {
        width: "100%",
    },
    iconSmall: {
        fontSize: 10,
    },
    button: {
        margin: theme.spacing.unit,
        width: 100,
        marginLeft: 445,
        marginTop: 15,
        padding: 10
    },
    formControl: {
        width: 520,
        marginLeft: 25,
        marginTop: 25,
        backgroundColor: "#009688",
        color: "#000000"
  },
  fo: {
    wordBreak: "break-all"
  }
});

@inject('MovieStore', 'SelfStore') @observer
class Movie extends Component {
    constructor(props) {
        super(props);
        this.renderPlayer = this.renderPlayer.bind(this);
    }

    componentDidMount() {
        this.props.MovieStore.fetchMovieDetails(this.props.match.params.id);
    }

    componentWillUnmount() {
        this.props.MovieStore.resetMovie();
    }

    renderPlayer(){
        console.log(this.props.SelfStore.self.locale);
        const { MovieStore, classes } = this.props;
        const { movie } = MovieStore;

        return (
            <Plyr
                type="video"
                title={movie.title}
                poster={`http://image.tmdb.org/t/p/w500/${movie.backdrop_path}`}
                sources={[
                    { src: `http://localhost:3200/film/${movie.imdb_id}/720`, type: 'video/mp4', size: "720" },
                    { src: `http://localhost:3200/film/${movie.imdb_id}/1080`, type: 'video/mp4', size: "1080" }
                ]}
            />
        )
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

                    <Grid container spacing={16} className={classes.container} direction="column" justify="center" alignItems="center">

                        <Grid item xs={12} md={6}>
                            <Paper>
                                <Grid container>
                                    <Grid item xs={5} className={classes.item}>
                                        <img className={classes.media} src={movie.poster_path} alt="Movie poster"/>
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

                        <Grid item xs={12} md={6}>
                            <Grid container>
                                <Paper>
                                    <Grid item className={classes.item}>
                                        { this.renderPlayer() }
                                    </Grid>
                                </Paper>
                            </Grid>
                        </Grid>

                        <Grid item xs={12} md={6}>
                            <Grid container>
                                <Paper>
                                    <FormControl  className={classes.formControl} variant="filled">
                                        <InputLabel htmlFor="component-filled">Your Comments</InputLabel>
                                        <FilledInput id="component-filled" />
                                    </FormControl>
                                    <Button variant="contained" color="primary" className={classes.button}>
                                        Send
                                        <Icon className={classes.rightIcon}>send</Icon>
                                    </Button>
                                </Paper>
                            </Grid>
                        </Grid>

                        <Grid item xs={12} md={6}>
                            <Grid container>
                                <Paper>
                                    <Grid item className={classes.item}>
                                        <Typography variant="p" component="p" className={classes.fo}>
                                            test
                                        </Typography>
                                    </Grid>
                                </Paper>
                            </Grid>
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