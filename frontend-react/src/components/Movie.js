import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';
import { TextField, Typography, CircularProgress, FilledInput, InputLabel, Icon, Button, FormControl} from '@material-ui/core';
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
    container2: {
        display: 'flex',
        flexWrap: 'wrap',
    },
    item : {
        margin: theme.spacing.unit
    },
    playerContainer: {
        minWidth: '100%',
        padding: theme.spacing.unit
    },
    media: {
        width: "100%",
    },
    iconSmall: {
        fontSize: 10,
    },
    button: {
        // margin: theme.spacing.unit,
        // width: 100,
        // marginLeft: 445,
        // marginTop: 15,
        // padding: 10
    },
    formControl: {
        width: "100%",
        // marginLeft: 25,
  },
  fo: {
    wordBreak: "break-all"
  },
  reactPlayer: {
      padding: 0,
      margin: 0
  }
});

@inject('MovieStore', 'SelfStore') @observer
class Movie extends Component {
    constructor(props) {
        super(props);
        this.state = {
            movieId: this.props.match.params.id,
            commentValue: '',
        };
        this.handleInput = this.handleInput.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
    }

    componentDidMount() {
        const { movieId } = this.state;
        this.props.MovieStore.fetchMovieDetails(movieId);
        this.props.MovieStore.fetchComments(movieId);
    }

    componentWillUnmount() {
        this.props.MovieStore.resetMovie();
    }

    handleInput(e) {
        this.setState({
            commentValue: e.target.value
        });
    }
    handleSubmit(e) {
        e.preventDefault();
        this.props.MovieStore.postComment(this.state.movieId, this.state.commentValue);
        this.setState({
            commentValue: ''
        });
    }

    renderPlayer(){
        console.log(this.props.SelfStore.self.locale);
        const { MovieStore, classes } = this.props;
        const { movie } = MovieStore;

        return (
            <Grid item xs={12} md={6}>
                <Paper square className={classes.playerContainer}>
                    <ReactPlayer
                        controls
                        className={classes.reactPlayer}
                        url={`http://localhost:3200/film/${movie.imdb_id}/720`}
                        // config={{
                        //     file: {
                        //         attributes: {
                        //             crossOrigin: 'true'
                        //         },
                        //         tracks: [
                        //             {
                        //                 kind: 'subtitles',
                        //                 src: `http://localhost:3200/subtitles/${movie.imdb_id}/en`,
                        //                 srcLang: 'en'
                        //             }
                        //         ]
                        //     }
                        // }}
                        width='100%'
                        height='100%'
                    />
                </Paper>
            </Grid>
        )
    }

    render() {
        const { classes } = this.props;
        const { movie, comments } = this.props.MovieStore;

        if (movie === undefined) {
            return <CircularProgress />;
        } else if (movie === null) {
            return "ERROR";
        } else {
            return (
                <main className={classes.layout}>

                    <Grid container spacing={16} className={classes.container} direction="column" justify="center" alignItems="center">

                        <Grid item xs={6} md={6}>
                            <Paper>
                                <Grid container>
                                    <Grid item xs={4} className={classes.item}>
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

                        { this.renderPlayer() }

                        <Grid item xs={6} md={6} >
                            <Grid container>
                                <Paper className={classes.formControl} >
                                     <form className={classes.container2} noValidate autoComplete="off">
                                        <TextField 
                                          value={this.state.commentValue}
                                          onChange={this.handleInput}
                                          style={{ margin: 8 }}
                                          placeholder="Comment"
                                          fullWidth
                                          margin="normal"
                                          InputLabelProps={{
                                            shrink: true,
                                          }}
                                        />    
                                        <Button
                                            onClick={this.handleSubmit}
                                            variant="contained"
                                            color="primary"
                                            className={classes.button}>
                                        Send
                                        <Icon className={classes.rightIcon}>send</Icon>
                                        </Button>
                                    </form>
                                </Paper>
                            </Grid>
                        </Grid>
                        

                        <Grid item xs={6} md={6} >
                            <Grid container>
                                <Paper>
                                    { comments.map( c => (
                                        <Grid item className={classes.item}>
                                            <p>{ c.uname }</p>
                                            <Typography
                                                variant="p"
                                                component="p"
                                                className={classes.fo}>
                                                { c.text }
                                            </Typography>
                                            <p>{ c.dt }</p>
                                        </Grid>
                                    ))}
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