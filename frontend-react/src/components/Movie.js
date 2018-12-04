import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';
import { TextField, Typography, CircularProgress, Button, Avatar, List, ListItem, ListItemAvatar, ListItemText, Icon } from '@material-ui/core';
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
    playerContainer: {
        minWidth: '100%',
        padding: theme.spacing.unit
    },
    media: {
        width: "100%",
    },
    reactPlayer: {
        padding: 0,
        margin: 0
    },
    container: {
        padding: theme.spacing.unit * 2
    },
    avatar: {
		backgroundColor: theme.palette.grey,
    },
    commentSection: {
        paddingTop: theme.spacing.unit * 2,
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
    overview: {
        marginTop: theme.spacing.unit,
        marginBottom: theme.spacing.unit
    },
    link: {
        textDecoration: 'none',
        color: 'white'
    }
});
@withNamespaces()
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
        this.resetCommentInput = this.resetCommentInput.bind(this);
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

    renderPlayer(){
        const { MovieStore, classes } = this.props;
        const { movie } = MovieStore;

        return (
            <Grid item>
                <Paper square className={classes.playerContainer}>
                    <ReactPlayer
                        controls
                        className={classes.reactPlayer}
                        url={movie.streaming}
                        config={{
                            file: {
                                attributes: {
                                    crossOrigin: "use-credentials"
                                },
                                tracks: [
                                    {
                                        kind: 'subtitles',
                                        src: `http://localhost:3200/subtitles/${movie.imdb_id}/720p/en`,
                                        srcLang: 'en'
                                    },
                                    {
                                        kind: 'subtitles',
                                        src: `http://localhost:3200/subtitles/${movie.imdb_id}/720p/ru`,
                                        srcLang: 'ru'
                                    }
                                ]
                            }
                        }}
                        width='100%'
                        height='100%'
                    />
                </Paper>
            </Grid>
        )
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
        const { commentValue } = this.state;
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

                    <Grid item xs md={6} className={classes.root}>
                        <Grid container spacing={16} className={classes.container} direction="column">

                            <Grid item>
                                <Paper>
                                    <Grid container>

                                        <Grid item xs={4} className={classes.item}>
                                            <img className={classes.media} src={movie.poster_path} alt="Movie poster"/>
                                        </Grid>

                                        <Grid item xs className={classes.item}>
                                            <Grid container direction="column">
                                                <Grid item>
                                                    <Typography variant="h6" gutterBottom>
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
                                                <Grid item>
                                                    <Typography variant="body1" className={classes.overview}>
                                                        {movie.overview}
                                                    </Typography>
                                                </Grid>
                                            </Grid>
                                        </Grid>

                                    </Grid>

                                    {
                                        !!movie.streaming &&
                                        <Grid container direction="column">
                                            {
                                                movie.streaming.map(streamUrl => (
                                                    <Grid key={streamUrl} item xs={4} className={classes.item}>
                                                        <Button 
                                                            variant="contained"
                                                            color="primary"
                                                            fullWidth
                                                        >
                                                            <Icon className={classes.leftIcon}>play_arrow</Icon>
                                                            { t('movie:streamIn') + ' ' + streamUrl.split('/').pop() }
                                                        </Button>
                                                    </Grid>
                                                ))
                                            }
                                        </Grid>
                                    }

                                </Paper>
                            </Grid>

                            {/* { !!movie.streaming && this.renderPlayer() } */}

                            <Grid item xs>
                                <Paper>
                                    <Grid container direction="column">

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
                                </Paper>
                            </Grid>

                        </Grid>
                    </Grid>

                </main>
            );
        }
    }
}

export default withStyles(styles)(Movie);