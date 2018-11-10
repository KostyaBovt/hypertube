import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import { Typography, Card, CardActionArea, CardMedia, CardContent, FormControl, InputLabel, Input, FormHelperText, InputAdornment, Icon, Button, IconButton, Select, Chip, MenuItem, ListItemText, Checkbox} from '@material-ui/core';

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
    card: {
        maxWidth: 250,
        margin: theme.spacing.unit
    },
    formControl: {
        minWidth: 120,
        maxWidth: 300,
    },
    chips: {
        display: 'flex',
        flexWrap: 'wrap',
    },
    chip: {
        margin: theme.spacing.unit / 4,
    },
    media: {
        objectFit: 'cover',
    }
});

const ITEM_HEIGHT = 48;
const ITEM_PADDING_TOP = 8;
const MenuProps = {
  PaperProps: {
    style: {
      maxHeight: ITEM_HEIGHT * 4.5 + ITEM_PADDING_TOP,
      width: 250,
    },
  },
};

const genres = [
    {"id":28,"name":"Action"},
    {"id":12,"name":"Adventure"},
    {"id":16,"name":"Animation"},
    {"id":35,"name":"Comedy"},
    {"id":80,"name":"Crime"},
    {"id":99,"name":"Documentary"},
    {"id":18,"name":"Drama"},
    {"id":10751,"name":"Family"},
    {"id":14,"name":"Fantasy"},
    {"id":36,"name":"History"},
    {"id":27,"name":"Horror"},
    {"id":10402,"name":"Music"},
    {"id":9648,"name":"Mystery"},
    {"id":10749,"name":"Romance"},
    {"id":878,"name":"Science Fiction"},
    {"id":10770,"name":"TV Movie"},
    {"id":53,"name":"Thriller"},
    {"id":10752,"name":"War"},
    {"id":37,"name":"Western"}
];


@inject('LibraryStore') @observer
class Library extends Component {
    constructor(props) {
		super(props);
        this.state = { isLoading: false, genres: [] }
        this.handleGenreSelect = this.handleGenreSelect.bind(this);
        this.renderGenreSelectValues = this.renderGenreSelectValues.bind(this);
        this.deleteSelectedGenre = this.deleteSelectedGenre.bind(this);
        this.fetchMoviesWithFilters = this.fetchMoviesWithFilters.bind(this);
    }

    handleFormSubmit(e) {
        e.preventDefault();
    }

    componentDidMount() {
        this.props.LibraryStore.fetchMovies();
    }

    fetchMoviesWithFilters() {
        this.props.LibraryStore.fetchMoviesWithFilters();
    }

    handleGenreSelect(e) {
        this.setState({ genres: e.target.value }, () => {
            const selectedGenres = this.state.genres.map(genreIndex => {
                return genres[genreIndex].id;
            });

            this.props.LibraryStore.setGenres(selectedGenres);
            this.fetchMoviesWithFilters();
        });
    }


    deleteSelectedGenre = genreIndex => () => {
        console.log(genreIndex);
        this.setState(state => {
            const genres = [...state.genres];
            const genreToDelete = genres.indexOf(genreIndex);
            genres.splice(genreToDelete, 1);
            return { genres };
        }, () => {
            const selectedGenres = this.state.genres.map(genreIndex => {
                return genres[genreIndex].id;
            });

            this.props.LibraryStore.setGenres(selectedGenres);
            this.fetchMoviesWithFilters();
        });
        this.fetchMoviesWithFilters();
    }

    renderGenreSelectValues(selectedGenres) {
        const { classes } = this.props;
        return (
            <div className={classes.chips}>
                {selectedGenres.map(genreIndex => (
                    <Chip
                        key={genreIndex}
                        label={genres[genreIndex].name}
                        className={classes.chip}
                        onDelete={this.deleteSelectedGenre(genreIndex)}
                    />
                ))}
            </div>
        );
    }

    render() {
        const { classes, LibraryStore } = this.props;
        const { movies, filters, queryString } = LibraryStore;
        console.log('render genres', this.state.genres);
        return (
            <main>
                <Grid container className={classes.layout} direction="column">

                    <Grid zeroMinWidth container className={classes.container} wrap="wrap" justify="center" alignItems="flex-end">

                        <Grid item>
                            <FormControl className={classes.formControl}>
                                <InputLabel htmlFor="select-multiple-chip">Genres</InputLabel>
                                <Select
                                    multiple
                                    value={this.state.genres}
                                    onChange={this.handleGenreSelect}
                                    input={<Input id="select-multiple-chip" />}
                                    renderValue={this.renderGenreSelectValues}
                                    MenuProps={MenuProps}
                                >
                                    {genres.map((genre, index) => (
                                        <MenuItem key={index} value={index}>
                                            <Checkbox checked={this.state.genres.includes(index)} />
                                            <ListItemText primary={genre.name} />
                                        </MenuItem>
                                    ))}
                                </Select>
                            </FormControl>
                        </Grid>

                        <Grid item>
                            <Grid container spacing={8} alignItems="flex-end">
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

                    <Grid container className={classes.container} wrap="wrap" justify="center" zeroMinWidth>
                        {   movies &&
                            movies.map(movie => (
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
            </main>
        );
    }
}

Library.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(Library);