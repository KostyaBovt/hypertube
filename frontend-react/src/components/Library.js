import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import { Typography, Card, CardActionArea, CardMedia, CardContent, FormControl, InputLabel, Input, InputAdornment, Icon, Button, Select, Chip, MenuItem, ListItemText, Checkbox, CircularProgress, FilledInput, TextField, IconButton, ButtonBase} from '@material-ui/core';

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
    filtersContainer: {
        marginTop: theme.spacing.unit * 2,
    },
    filterItem: {
        margin: theme.spacing.unit * 2
    },
    card: {
        width: 250,
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
    },
    loadMoreButton: {

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
        this.state = {
            isLoading: false,
            genres: [],
            searchMode: false // Very strange thing that i need to think about
        }
        this.handleSearchFormSubmit = this.handleSearchFormSubmit.bind(this);
        this.handleGenreSelect = this.handleGenreSelect.bind(this);
        this.renderGenreSelectValues = this.renderGenreSelectValues.bind(this);
        this.deleteSelectedGenre = this.deleteSelectedGenre.bind(this);
        this.handleFilterChange = this.handleFilterChange.bind(this);
        this.clearSearchInput = this.clearSearchInput.bind(this);
        this.renderMainContent = this.renderMainContent.bind(this);
        this.renderMovies = this.renderMovies.bind(this);
        this.fetchMovies = this.fetchMovies.bind(this);
        this.handleMovieSearch = this.handleMovieSearch.bind(this);
        this.handleLoadMore = this.handleLoadMore.bind(this);
    }

    handleSearchFormSubmit(e) {
        e.preventDefault();
        this.handleMovieSearch();
    }

    componentDidMount() {
        this.props.LibraryStore.fetchMovies();
    }

    fetchMovies(page = 1) {
        this.props.LibraryStore.fetchMovies(page);
    }

    handleMovieSearch(page = 1) {
        this.props.LibraryStore.fetchSearchResults(page);
        this.setState({ searchMode: true });
    }
    
    handleLoadMore(currentPage) {
        if (this.state.searchMode) {
            this.handleMovieSearch(currentPage + 1)
        } else {
            this.fetchMovies(currentPage + 1);
        }
    }

    clearSearchInput() {
        this.props.LibraryStore.setQueryString("");
        this.setState({ searchMode: false })
        this.fetchMovies();
    }

    handleGenreSelect(e) {
        console.log('handle select', e.target.value);
        this.setState({ genres: e.target.value }, () => {
            const selectedGenres = this.state.genres.map(genreIndex => {
                return genres[genreIndex].id;
            });
            this.props.LibraryStore.setGenres(selectedGenres);
            this.fetchMovies();
        });
    }

    handleFilterChange(e) {
        const { name, value } = e.target;
        console.log('setting filter', name, value);
        this.props.LibraryStore.setFilter(name, value);
        this.fetchMovies();
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
            this.fetchMovies();
        });
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

    renderMovies(movies) {
        const { classes } = this.props;
        return movies.map((movie, index) => (
            <Grid item key={movie.id}>
                <Card className={classes.card}>
                    <CardActionArea href={`/movie/${movie.id}`}>
                        <CardMedia
                            component="img"
                            className={classes.media}
                            alt={movie.title}
                            height="375"
                            image={movie.poster_path || 'http://www.theprintworks.com/wp-content/themes/psBella/assets/img/film-poster-placeholder.png'}
                            title={movie.title}
                        />
                        <CardContent>
                            <Typography variant="subtitle2" noWrap>
                                {movie.title}
                            </Typography>
                            <Typography variant="subtitle2" gutterBottom color="textSecondary">
                                { movie.release_date.split("-")[0] || 'Release date unknown' }
                            </Typography>
                            <Typography variant="caption" color="textSecondary">
                                IMDb rating - {movie.vote_average}/10
                            </Typography>
                        </CardContent>
                    </CardActionArea>
                </Card>
            </Grid>
        ));
    }

    renderMainContent(movies) {
        if (movies && movies.length > 0) {
            return this.renderMovies(movies);
        } else if (movies && movies.length === 0) {
            return (
                <Typography variant="subtitle2" color="textSecondary">
                    No results found
                </Typography>
            );
        } else if (movies === undefined) {
            return <CircularProgress />;
        } else if (movies === null) {
            return (
                <Typography variant="subtitle2" color="textSecondary">
                    Error occured, try again
                </Typography>
            );
        }
    }

    render() {
        const { classes, LibraryStore } = this.props;
        const { isLoading, movies, currentPage, totalPages, filters, queryString } = LibraryStore;
        return (
            <main>
                <Grid container className={classes.layout} direction="column">

                    <Grid container className={classes.filtersContainer} wrap="wrap" justify="center" alignItems="flex-end">

                        <Grid item className={classes.filterItem}>
                            <FormControl disabled={!!queryString} variant="filled" className={classes.formControl}>
                                <InputLabel htmlFor="sort_by">Sort by</InputLabel>
                                <Select
                                    value={filters.sort_by}
                                    onChange={this.handleFilterChange}
                                    input={<FilledInput name="sort_by" id="sort_by" />}
                                >
                                    <MenuItem value={"popularity.asc"}>Popularity ascending</MenuItem>
                                    <MenuItem value={"popularity.desc"}>Popularity descending</MenuItem>
                                    <MenuItem value={"vote_average.asc"}>Rating ascending</MenuItem>
                                    <MenuItem value={"vote_average.desc"}>Rating descending</MenuItem>
                                    <MenuItem value={"primary_release_date.asc"}>Release date ascending</MenuItem>
                                    <MenuItem value={"primary_release_date.desc"}>Release date descending</MenuItem>
                                    <MenuItem value={"revenue.asc"}>Revenue ascending</MenuItem>
                                    <MenuItem value={"revenue.desc"}>Revenue descending</MenuItem>
                                </Select>
                            </FormControl>
                        </Grid>

                        <Grid item className={classes.filterItem}>
                            <FormControl disabled={!!queryString} className={classes.formControl}>
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

                        {/* <Grid item>
                            <Grid container spacing={8} alignItems="flex-end"> */}
                                <Grid item className={classes.filterItem}>
                                    <form onSubmit={this.handleSearchFormSubmit}>
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
                                                endAdornment={
                                                    queryString &&
                                                    <InputAdornment position="end">
                                                        <ButtonBase disableRipple onClick={this.clearSearchInput}>
                                                            <Icon fontSize="small">close</Icon>
                                                        </ButtonBase>
                                                    </InputAdornment>
                                                }
                                                onChange={(e) => LibraryStore.setQueryString(e.target.value)}
                                            />
                                        </FormControl>
                                    </form>
                                </Grid>
                                {/* <Grid item>
                                    <Button color="primary" className={classes.button}>
                                        Go
                                    </Button>
                                </Grid>
                            </Grid>
                        </Grid> */}

                    </Grid>

                    <Grid container className={classes.container} wrap="wrap" justify="center" zeroMinWidth>
                        { this.renderMainContent(movies) }
                    </Grid>

                    {
                        movies && !isLoading && currentPage < totalPages &&
                        <Grid container className={classes.container} justify="center">
                            <Grid item>
                                <Button onClick={() => this.handleLoadMore(currentPage)} variant="outlined" size="large" color="primary">
                                    Load more
                                </Button>
                            </Grid>
                        </Grid>
                    }

                </Grid>
            </main>
        );
    }
}

Library.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(Library);