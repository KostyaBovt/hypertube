import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import { Typography, Card, CardActionArea, CardMedia, CardContent, FormControl, InputLabel, Input, InputAdornment, Icon, Button, Select, Chip, MenuItem, ListItemText, Checkbox, CircularProgress, ButtonBase} from '@material-ui/core';
import { Link } from 'react-router-dom'

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
    backButton: {
        margin: theme.spacing.unit
    },
    yearInput: {
        textAlign: 'center'
    },
    divider: {
        margin: `0px ${theme.spacing.unit * 8}px`
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
            genres: [], // TODO: fix filters state after changing page!!
            queryString: ""
        }
        this.handleSearchFormSubmit = this.handleSearchFormSubmit.bind(this);
        this.handleGenreSelect = this.handleGenreSelect.bind(this);
        this.renderGenreSelectValues = this.renderGenreSelectValues.bind(this);
        this.deleteSelectedGenre = this.deleteSelectedGenre.bind(this);
        this.handleFilterChange = this.handleFilterChange.bind(this);
        this.disableSearchMode = this.disableSearchMode.bind(this);
        this.renderHelpers = this.renderHelpers.bind(this);
        this.renderMovies = this.renderMovies.bind(this);
        this.renderLoadMoreButton = this.renderLoadMoreButton.bind(this);
        this.fetchMovies = this.fetchMovies.bind(this);
        this.handleMovieSearch = this.handleMovieSearch.bind(this);
        this.handleLoadMore = this.handleLoadMore.bind(this);
    }

    handleSearchFormSubmit(e) {
        e.preventDefault();
        
        const { LibraryStore } = this.props;
        const { queryString } = this.state;

        LibraryStore.resetFilters();
        LibraryStore.resetMovies();
        LibraryStore.setSearchMode(true);
        LibraryStore.setCurrentQuery(queryString);

        this.setState({ genres: [] });
        this.handleMovieSearch();
    }

    componentDidMount() {
        const { LibraryStore } = this.props;
        if (!LibraryStore.movies) {
            this.props.LibraryStore.fetchMovies();
        } else {
            
        }
    }

    fetchMovies(page = 1) {
        this.props.LibraryStore.fetchMovies(page);
    }

    handleMovieSearch(page = 1) {
        this.props.LibraryStore.fetchSearchResults(page);
    }
    
    handleLoadMore(currentPage) {
        if (this.props.LibraryStore.searchMode) {
            this.handleMovieSearch(currentPage + 1)
        } else {
            this.fetchMovies(currentPage + 1);
        }
    }

    disableSearchMode() {
        const { LibraryStore } = this.props;

        LibraryStore.setSearchMode(false);
        LibraryStore.setCurrentQuery(undefined);
        LibraryStore.resetMovies();

        this.fetchMovies();
    }

    handleGenreSelect(e) {
        // console.log('handle select', e.target.value);
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

    renderHelpers(movies) {
        const { searchMode, currentQuery } = this.props.LibraryStore;
        if (movies === null) {
            return (
                <Grid item>
                    <Typography variant="subtitle2" color="textSecondary">
                        Error occured, try again
                    </Typography>
                </Grid>
            );
        } else if (!searchMode && movies && movies.length === 0) {
            return (
                <Grid item>
                    <Typography variant="subtitle2" color="textSecondary">
                        No results found
                    </Typography>
                </Grid>
            );
        } else if (searchMode && movies && movies.length === 0) {
            return (
                <Grid item>
                    <Typography variant="subtitle2" color="textSecondary">
                        No results found for "{currentQuery}"
                    </Typography>
                </Grid>
            );
        } else if (searchMode && movies) {
            return (
                <Grid item>
                    <Typography variant="subtitle2" color="textSecondary">
                        Search results for "{currentQuery}"
                    </Typography>
                </Grid>
            );
        }
    }

    renderMovieRating(movie) {
        if (movie.vote_count > 0) {
            return (
                <Typography variant="caption" color="textSecondary">
                    IMDb rating - {movie.vote_average}/10
                </Typography>
            );
        } else {
            return (
                <Typography variant="caption" color="textSecondary">
                    No rating
                </Typography>
            );
        }
    }
    
    renderMovies(movies) {
        if (!movies) return;
        const { classes } = this.props;
        return movies.map(movie => (
            <Grid item key={movie.id}>
                <Card className={classes.card}>
                    <CardActionArea component={Link} to={`/movie/${movie.id}`}>
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
                            { this.renderMovieRating(movie) }
                        </CardContent>
                    </CardActionArea>
                </Card>
            </Grid>
        ));
    }

    renderLoadMoreButton() {
        const { classes, LibraryStore } = this.props;
        const { isLoading, movies, currentPage, totalPages } = LibraryStore;

        if (isLoading) {
            return (
                <Grid container className={classes.container} justify="center">
                    <Grid item>
                        <CircularProgress/>
                    </Grid>
                </Grid>
            );
        } else if (movies && !isLoading && currentPage < totalPages) {
            return (
                <Grid container className={classes.container} justify="center">
                    <Grid item>
                        <Button onClick={() => this.handleLoadMore(currentPage)} variant="outlined" size="large" color="primary">
                            Load more
                        </Button>
                    </Grid>
                </Grid>
            );
        }
    }

    render() {
        const { queryString } = this.state;
        const { classes, LibraryStore } = this.props;
        const { isLoading, movies, filters, searchMode } = LibraryStore;
        return (
            <main>
                <Grid container className={classes.layout} direction="column">

                    <Grid container className={classes.filtersContainer} wrap="wrap" justify="space-around" alignItems="flex-end">

                        <Grid item>
                            <Grid container justify="center" alignItems="flex-end">
                                <Grid item className={classes.filterItem}>
                                    <FormControl disabled={searchMode} className={classes.formControl}>
                                        <InputLabel htmlFor="sort_by">Sort by</InputLabel>
                                        <Select
                                            value={filters.sort_by}
                                            onChange={this.handleFilterChange}
                                            input={<Input name="sort_by" id="sort_by" />}
                                        >
                                            <MenuItem value={"popularity.asc"}>Popularity ascending</MenuItem>
                                            <MenuItem value={"popularity.desc"}>Popularity descending</MenuItem>
                                            <MenuItem value={"vote_average.asc"}>Rating ascending</MenuItem>
                                            <MenuItem value={"vote_average.desc"}>Rating descending</MenuItem>
                                            <MenuItem value={"primary_release_date.asc"}>Release date ascending</MenuItem>
                                            <MenuItem value={"primary_release_date.desc"}>Release date descending</MenuItem>
                                            <MenuItem value={"revenue.asc"}>Revenue ascending</MenuItem>
                                            <MenuItem value={"revenue.desc"}>Revenue descending</MenuItem>
                                            <MenuItem value={"original_title.asc"}>Title ascending</MenuItem>
                                            <MenuItem value={"original_title.desc"}>Title descending</MenuItem>
                                            
                                        </Select>
                                    </FormControl>
                                </Grid>

                                <Grid item className={classes.filterItem}>
                                    <FormControl disabled={searchMode} className={classes.formControl}>
                                        <InputLabel htmlFor="select-multiple-chip">Genres</InputLabel>
                                        <Select
                                            multiple
                                            value={this.state.genres}
                                            onChange={this.handleGenreSelect}
                                            input={<Input id="select-multiple-chip" />}
                                            renderValue={this.renderGenreSelectValues}
                                            MenuProps={MenuProps}
                                        >
                                        {
                                            genres.map((genre, index) => (
                                            <MenuItem key={index} value={index}>
                                                <Checkbox checked={this.state.genres.includes(index)} />
                                                <ListItemText primary={genre.name} />
                                            </MenuItem>
                                            ))
                                        }
                                        </Select>
                                    </FormControl>
                                </Grid>

                                <Grid item className={classes.filterItem}>
                                    <FormControl disabled={searchMode} className={classes.formControl}>
                                        <InputLabel htmlFor="vote_average">Rating</InputLabel>
                                        <Select
                                            value={filters["vote_average_gte"]}
                                            onChange={this.handleFilterChange}
                                            input={<Input name="vote_average_gte" id="vote_average" />}
                                        >
                                            <MenuItem value="">
                                                <em>Any</em>
                                            </MenuItem>
                                        {
                                            ["1+","2+","3+","4+","5+","6+","7+","8+","9+"].map((value, index) => (
                                                <MenuItem key={index} value={value.charAt(0)}>{value}</MenuItem>
                                            ))
                                        }
                                        </Select>
                                    </FormControl>
                                </Grid>

                                <Grid item className={classes.filterItem}>
                                    <Grid container spacing={8} alignItems="center">
                                        <Grid item>
                                            <Input
                                                classes={{ input: classes.yearInput }}
                                                inputProps={{size: 6}}
                                                id="year-grater"
                                                type="text"
                                                name="release_date.gte"
                                                placeholder="Year"
                                                
                                                // value={queryString}
                                                // onChange={(e) => this.setState({queryString: e.target.value})}
                                            />
                                        </Grid>
                                        <Grid item>
                                            <Typography variant="subtitle2" color="textSecondary">
                                                –
                                            </Typography>
                                        </Grid>
                                        <Grid item>
                                            <Input
                                                classes={{ input: classes.yearInput }}
                                                inputProps={{size: 6}}
                                                id="year-lower"
                                                type="text"
                                                name="release_date.lte"
                                                placeholder="Year"
                                                // value={queryString}
                                                // onChange={(e) => this.setState({queryString: e.target.value})}
                                            />
                                        </Grid>
                                    </Grid>
                                </Grid>
                            </Grid>
                        </Grid>

                        <Grid item className={classes.filterItem}>
                            <form onSubmit={this.handleSearchFormSubmit}>
                                <FormControl>
                                    <Input
                                        id="search-field"
                                        type="text"
                                        name="queryString"
                                        placeholder="Search..."
                                        value={queryString}
                                        startAdornment={
                                            <InputAdornment position="start">
                                                    <Icon>search</Icon>
                                            </InputAdornment>
                                        }
                                        onChange={(e) => this.setState({queryString: e.target.value})}
                                    />
                                </FormControl>
                            </form>
                        </Grid>

                    </Grid>
                    
                    <Grid container className={classes.filtersContainer} wrap="wrap" justify="center" alignItems="center" zeroMinWidth>
                        { this.renderHelpers(movies, isLoading) }
                        {
                            searchMode && !isLoading &&
                            <Grid item className={classes.backButton}>
                                <ButtonBase disableRipple onClick={this.disableSearchMode}>
                                    <Icon color="action">close</Icon>
                                </ButtonBase>
                            </Grid>
                        }
                    </Grid>

                    <Grid container className={classes.container} wrap="wrap" justify="center" zeroMinWidth>
                        { this.renderMovies(movies) }
                    </Grid>

                    { this.renderLoadMoreButton() }

                </Grid>
            </main>
        );
    }
}

Library.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(Library);