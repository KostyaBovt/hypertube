import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import { Typography, Card, CardActionArea, CardMedia, CardContent, FormControl, InputLabel, Input, FormHelperText, InputAdornment, Icon, Button} from '@material-ui/core';

const styles = theme => ({
    layout: {
		width: 'auto',
		[theme.breakpoints.up(1600)]: {
			width: 1600,
			marginLeft: 'auto',
			marginRight: 'auto',
		}
    },
    container: {
        marginTop: theme.spacing.unit * 2,
		marginBottom: theme.spacing.unit * 2,
    },
    card: {
        maxWidth: 250,
        margin: theme.spacing.unit
    },
    formControl: {
        maxWidth: 80,
        margin: theme.spacing.unit
    },
    media: {
        objectFit: 'cover',
    }
  });


@inject('LibraryStore') @observer
class Library extends Component {
    constructor(props) {
		super(props);
		this.state = { isLoading: false }
    }

    handleFormSubmit(e) {
        e.preventDefault();
    }

    componentDidMount() {
        this.props.LibraryStore.fetchMovies();
    }

    render() {
        const { classes, LibraryStore } = this.props;
        return (
            <main className={classes.layout}>
                <Grid container className={classes.container} direction="column">
                    <Grid item>
                        <Grid container className={classes.container} wrap="wrap" justify="center" alignItems="center">
                            <Grid item>
                                <Grid container className={classes.container} spacing={8}>
                                    <Grid item>
                                        <FormControl>
                                            <Input
                                                id="search-field"
                                                type="text"
                                                name="queryString"
                                                placeholder="Search..."
                                                value={LibraryStore.queryString}
                                                startAdornment={
                                                    <InputAdornment position="start">
                                                        <Icon>search</Icon>
                                                    </InputAdornment>
                                                }
                                                onChange={(e) => LibraryStore.setQueryString(e.target.value)}
                                            />
                                            <FormHelperText></FormHelperText>
                                        </FormControl>
                                    </Grid>
                                    <Grid item>
                                        <Button color="primary" className={classes.button}>
                                            Go
                                        </Button>
                                    </Grid>
                                </Grid>
                            </Grid>
                        </Grid>
                    </Grid>

                    <Grid item>
                        <Grid container className={classes.container} wrap="wrap" justify="center">
                            {   LibraryStore.movies &&
                                LibraryStore.movies.map(movie => (
                                    <Grid item key={movie.id}>
                                        <Card className={classes.card}>
                                            <CardActionArea href={`/movie/${movie.id}`}>
                                                <CardMedia
                                                    component="img"
                                                    className={classes.media}
                                                    alt={movie.title}
                                                    height="375"
                                                    image={movie.poster_path}
                                                    title={movie.title}
                                                />
                                                <CardContent>
                                                    <Typography variant="subtitle2" noWrap>
                                                        {movie.title}
                                                    </Typography>
                                                    <Typography variant="subtitle2" gutterBottom color="textSecondary">
                                                        { movie.release_date.split("-")[0] }
                                                    </Typography>
                                                    <Typography variant="caption" color="textSecondary">
                                                        IMDb rating - {movie.vote_average}/10
                                                    </Typography>
                                                </CardContent>
                                            </CardActionArea>
                                        </Card>
                                    </Grid>
                                ))
                            }
                        </Grid>
                    </Grid>
                </Grid>
            </main>
        );
    }
}

Library.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(Library);