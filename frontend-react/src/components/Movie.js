import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';
import { TextField, Typography, CircularProgress, Button, List, ListItem, ListItemAvatar, ListItemText, Icon, Divider } from '@material-ui/core';
import ReactPlayer from 'react-player';
import imgHelpers from '../helpers/imgHelpers';
import { Link } from 'react-router-dom';
import { withNamespaces } from 'react-i18next';

import { distanceInWordsToNow } from 'date-fns';

const styles = theme => ({
    root: {
        marginTop: theme.spacing.unit * 2,
        marginBottom: theme.spacing.unit * 2,
        marginLeft: 'auto',
        marginRight: 'auto',
    },
    item: {
        margin: theme.spacing.unit
    },
    media: {
        width: '100%'
    },
    playerContainer: {
        position: 'relative',
        paddingTop: 'calc((9 / 16) * 100%)',
        backgroundColor: '#000',
    },
    reactPlayer: {
        position: 'absolute',
        top: 0,
        left: 0
    },
    container: {
        padding: theme.spacing.unit * 2
    },
    movieDetails: {
        minWidth: '300px',
        margin: theme.spacing.unit
    },
    commentForm: {
        paddingTop: theme.spacing.unit * 2,
        paddingLeft: theme.spacing.unit * 2,
        paddingRight: theme.spacing.unit * 2,
        [theme.breakpoints.up(600)]: {
            paddingLeft: theme.spacing.unit * 3,
            paddingRight: theme.spacing.unit * 3,
		}
    },
    commentText: {
        wordWrap: 'break-word'
    },
    leftIcon: {
        marginRight: theme.spacing.unit,
    },
    inline: {
        display: 'inline',
    },
    link: {
        textDecoration: 'none',
        color: 'white'
    },
    divider: {
        marginLeft: theme.spacing.unit * 3,
        marginRight: theme.spacing.unit * 3
    }
});

@withNamespaces()
@inject('MovieStore', 'SelfStore', 'LibraryStore') @observer
class Movie extends Component {
    constructor(props) {
        super(props);
        this.state = {
            movieId: this.props.match.params.id,
            commentValue: '',
            streamingUrl: undefined,
            resolution: undefined
        };
        this.handleInput = this.handleInput.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
        this.handleSelection = this.handleSelection.bind(this);
        this.resetCommentInput = this.resetCommentInput.bind(this);
        this.handleSelection = this.handleSelection.bind(this);
    }

    componentDidMount() {
        const { movieId } = this.state;
        const { MovieStore } = this.props;

        MovieStore.fetchMovieDetails(movieId);
        MovieStore.fetchComments(movieId);
    }

    componentWillUnmount() {
        this.props.MovieStore.resetMovie();
    }

    handleInput(e) {
        this.setState({
            commentValue: e.target.value
        });
    }

    resetCommentInput() {
        this.setState({ commentValue: '' });
    }

    handleSubmit(e) {
        e.preventDefault();

        const { MovieStore } = this.props;
        const { movieId, commentValue } = this.state;

        MovieStore.postComment(movieId, commentValue);
        this.resetCommentInput();
    }

    handleSelection(url, resolution) {
        const { LibraryStore } = this.props;
        this.setState({ streamingUrl: url, resolution });
        LibraryStore.setSelectedMovieAsWatched();
    }

    renderPlayer() {
        const { MovieStore, classes } = this.props;
        const { movie } = MovieStore;
        const { streamingUrl, resolution } = this.state;

        if (streamingUrl || movie.trailer) {
            return (
                <Grid item className={classes.playerContainer}>
                    <ReactPlayer
                        controls
                        className={classes.reactPlayer}
                        url={streamingUrl || movie.trailer}
                        poster={movie.backdrop_path}
                        config={{
                            file: {
                                attributes: {
                                    crossOrigin: "use-credentials"
                                },
                                tracks: [
                                    {
                                        kind: 'subtitles',
                                        src: `http://localhost:3200/subtitles/${movie.imdb_id}/${resolution}/en`,
                                        srcLang: 'en'
                                    },
                                    {
                                        kind: 'subtitles',
                                        src: `http://localhost:3200/subtitles/${movie.imdb_id}/${resolution}/ru`,
                                        srcLang: 'ru'
                                    }
                                ]
                            }
                        }}
                        width='100%'
                        height='100%'
                    />
                </Grid>
            );
        }
    }

    renderStreamingButtons() {
        const { classes, t, MovieStore } = this.props;
        const { movie } = MovieStore;
        

        if (movie && movie.streaming) {
            const buttons = movie.streaming.map(streamUrl => {
                const resolution = streamUrl.split('/').pop();
                return (
                    <Grid key={streamUrl} item className={classes.item}>
                        <Button 
                            variant="contained"
                            color="primary"
                            onClick={() => this.handleSelection(streamUrl, resolution)}
                        >
                            <Icon className={classes.leftIcon}>play_arrow</Icon>
                            { `${t('movie:streamIn')} ${resolution}` }
                        </Button>
                    </Grid>
                );
            });

            return (
                <Grid item className={classes.item}>
                    <Grid container justify="center">
                        {buttons}
                    </Grid>
                </Grid>
            );
        }
    }

    renderCommentSectionActions(classes) {
        const { t } = this.props;
        return (
            <Grid container justify="flex-end">
                <Grid item className={classes.item}>
                    <Button
                        onClick={this.resetCommentInput}
                        variant="text"
                    >
                        {t('movie:cancel')}
                    </Button>
                </Grid>
                <Grid item className={classes.item}>
                    <Button
                        onClick={this.handleSubmit}
                        variant="contained"
                        color="primary"
                    >
                        {t('movie:send')}
                    </Button>
                </Grid>
            </Grid>
        );
    }

    render() {
        const { classes, t } = this.props;
        const { commentValue, streamingUrl } = this.state;
        const { self } = this.props.SelfStore; 
        const { movie, comments } = this.props.MovieStore;

        if (movie === undefined) {
            return (
                <main>
                    <Grid container className={classes.container} justify="center">
                        <Grid item>
                            <CircularProgress />
                        </Grid>
                    </Grid>
                </main>
            );
        } else if (movie === null) {
            return (
                <main>
                    <Grid container className={classes.container} justify="center">
                        <Grid item>
                            <Typography variant="subtitle2" color="textSecondary">
                                {t('movie:unavailable')}
                            </Typography>
                        </Grid>
                    </Grid>
                </main>
            );
        } else {
            return (
                <main>

                    <Grid item xs md={8} className={classes.root}>
                        <Paper>
                            <Grid container spacing={0} direction="column">

                                <Grid container justify="center" direction="column" className={classes.container}>

                                    <Grid container item justify="center">

                                            <Grid item md={5} xl={4} zeroMinWidth className={classes.item}>
                                                <div>
                                                    <img className={classes.media} src={movie.poster_path} alt="Movie poster"/>
                                                </div>
                                            </Grid>

                                            <Grid item xs className={classes.movieDetails}>
                                                <Grid container direction="column">
                                                    <Grid item>
                                                        <Typography variant="h5" gutterBottom>
                                                            {movie.title}
                                                        </Typography>
                                                    </Grid>
                                                    <Grid item>
                                                        <Typography className={classes.inline} variant="subtitle2" color="textPrimary">
                                                            {t('movie:originalTitle')}
                                                        </Typography>
                                                        <Typography className={classes.inline} variant="body2" color="textPrimary">
                                                            {movie.original_title}
                                                        </Typography>
                                                    </Grid>
                                                    <Grid item>
                                                        <Typography className={classes.inline} variant="subtitle2" color="textPrimary">
                                                            {t('movie:releaseDate')}
                                                        </Typography>
                                                        <Typography className={classes.inline} variant="body2" color="textPrimary">
                                                            { movie.release_date }
                                                        </Typography>
                                                    </Grid>
                                                    <Grid item>
                                                        <Typography className={classes.inline} variant="subtitle2" color="textPrimary">
                                                            {t('movie:runtime')}
                                                        </Typography>
                                                        <Typography className={classes.inline} variant="body2" color="textPrimary">
                                                            {movie.runtime + ' ' + t('movie:minutes')}
                                                        </Typography>
                                                    </Grid>
                                                    <Grid item>
                                                        <Typography className={classes.inline} variant="subtitle2" color="textPrimary">
                                                            {t('movie:rating')}
                                                        </Typography>
                                                        <Typography className={classes.inline} variant="body2" color="textPrimary">
                                                            {movie.vote_average}
                                                        </Typography>
                                                    </Grid>
                                                    <Grid item>
                                                        <Typography className={classes.inline} variant="subtitle2" color="textPrimary">
                                                            {t('movie:directors')}
                                                        </Typography>
                                                        <Typography className={classes.inline} variant="body2" color="textPrimary">
                                                            {movie.credits.crew.directors.join(', ')}
                                                        </Typography>
                                                    </Grid>
                                                    <Grid item>
                                                        <Typography className={classes.inline} variant="subtitle2" color="textPrimary">
                                                            {t('movie:producers')}
                                                        </Typography>
                                                        <Typography className={classes.inline} variant="body2" color="textPrimary">
                                                            {movie.credits.crew.producers.join(', ')}
                                                        </Typography>
                                                    </Grid>
                                                    <Grid item>
                                                        <Typography className={classes.inline} variant="subtitle2" color="textPrimary">
                                                            {t('movie:mainCast')}
                                                        </Typography>
                                                        <Typography className={classes.inline} variant="body2" color="textPrimary">
                                                            {movie.credits.main_cast.join(', ')}
                                                        </Typography>
                                                    </Grid>
                                                </Grid>
                                            </Grid>

                                    </Grid>

                                    {
                                        !!movie.overview &&
                                        <Grid item className={classes.item}>
                                            <Typography variant="body1">
                                                {movie.overview}
                                            </Typography>
                                        </Grid>
                                    }

                                    { this.renderStreamingButtons() }

                                </Grid>
                                
                                
                                { this.renderPlayer() }

                                { !streamingUrl && !movie.trailer && <Divider className={classes.divider} /> }

                                <Grid container direction="column" className={classes.container}>

                                    <Grid item className={classes.commentForm}>
                                        <form noValidate autoComplete="off">
                                            <Grid container spacing={16} alignItems="flex-start">
                                                <Grid item>
                                                    {  imgHelpers.renderAvatar(self, classes) }
                                                </Grid>
                                                <Grid item xs>
                                                    <TextField
                                                        value={this.state.commentValue}
                                                        onChange={this.handleInput}
                                                        placeholder={t('movie:liveComment')}
                                                        rowsMax={4}
                                                        fullWidth
                                                        multiline
                                                    />
                                                </Grid>
                                                { commentValue && this.renderCommentSectionActions(classes) }
                                            </Grid>
                                        </form>
                                    </Grid>

                                    <Grid item xs>
                                        <List>
                                        {
                                            comments.map((comment, i) => (
                                                <ListItem key={i} alignItems="flex-start">
                                                    <Link className={classes.link} to={"/user/" + comment.uname}>
                                                        <ListItemAvatar>
                                                            { imgHelpers.renderAvatar(comment, classes) }
                                                        </ListItemAvatar>
                                                    </Link>
                                                    <ListItemText
                                                        disableTypography
                                                        primary={
                                                            <Typography component="span" variant="subtitle2" color="textPrimary">
                                                                { comment.uname }
                                                            </Typography>
                                                        }
                                                        secondary={
                                                            <React.Fragment>
                                                                <Typography className={classes.commentText} component="span" variant="subtitle1" color="textPrimary">
                                                                    { comment.text }
                                                                </Typography>
                                                                <Typography component="span" variant="body2" color="textSecondary">
                                                                    { distanceInWordsToNow(new Date(comment.dt) + "UTC" , { addSuffix: true })}
                                                                </Typography>
                                                            </React.Fragment>
                                                        }
                                                    />
                                                </ListItem>
                                            ))
                                        }
                                        </List>
                                    </Grid>

                                </Grid>

                            </Grid>
                        </Paper>
                    </Grid>

                </main>
            );
        }
    }
}

export default withStyles(styles)(Movie);