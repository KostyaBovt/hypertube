import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';
import { TextField, Typography, CircularProgress, Button, Avatar} from '@material-ui/core';
import ReactPlayer from 'react-player';
import Plyr from 'react-plyr';

const styles = theme => ({
    layout: {
        marginLeft: 'auto',
        marginRight: 'auto',
        marginTop: theme.spacing.unit * 2,
		marginBottom: theme.spacing.unit * 2,
    },
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
    paper: {
        padding: theme.spacing.unit * 2
    },
    avatar: {
		margin: theme.spacing.unit,
		backgroundColor: theme.palette.grey,
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

    renderCommentSectionActions() {
        return (
            <Grid container justify="flex-end">
                <Grid item>
                    <Button
                        onClick={this.resetCommentInput}
                        variant="text"
                    >
                        Cancel
                    </Button>
                </Grid>
                <Grid item>
                    <Button
                        onClick={this.handleSubmit}
                        variant="text"
                        color="primary"
                    >
                        Send
                    </Button>
                </Grid>
            </Grid>
        );
    }

    render() {
        const { classes } = this.props;
        const { commentValue } = this.state;
        const { self } = this.props.SelfStore; 
        const { movie, comments } = this.props.MovieStore;

        if (movie === undefined) {
            return <CircularProgress />;
        } else if (movie === null) {
            return "ERROR";
        } else {
            return (
                <main>

                    <Grid item xs={12} md={6} className={classes.root}>
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

                            { !!movie.streaming && this.renderPlayer() }

                            <Grid item>
                                <Paper className={classes.paper} >
                                    <form noValidate autoComplete="off">
                                        <Grid container alignItems="flex-start">
                                            <Grid item>
                                                {  this.renderAvatar(self, classes) }
                                            </Grid>
                                            <Grid item xs>
                                                <TextField
                                                    value={this.state.commentValue}
                                                    onChange={this.handleInput}
                                                    placeholder="Leave a comment"
                                                    margin="dense"
                                                    fullWidth
                                                    multiline
                                                />
                                            </Grid>
                                        </Grid>
                                        { commentValue && this.renderCommentSectionActions() }
                                    </form>
                                </Paper>
                            </Grid>
                            

                            <Grid item>
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
                    </Grid>

                </main>
            );
        }
    }
}

export default withStyles(styles)(Movie);