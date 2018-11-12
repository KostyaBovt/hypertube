import { observable, action, toJS } from "mobx";
import axios from 'axios';

class LibraryStore {
    @observable isLoading = false;
    @observable movies = undefined;
    @observable currentPage = undefined;
    @observable totalPages = undefined;
    @observable filters = {
        sort_by: "popularity.desc",
        release_date: undefined,
        vote_average: undefined,
        with_genres: undefined
    }

    @observable queryString = '';

    @action setIsLoading(status) {
        this.isLoading = status;
    }
    
    @action setPage(page) {
        this.currentPage = page;
    }

    @action setTotalPages(pages) {
        this.totalPages = pages;
    }

    @action setFilter(name, value) {
        this.filters[name] = value;
    }

    @action setMovies(movies) {
        this.movies = movies;
    }

    @action pushMovies(moreMovies) {
        this.movies.push(...moreMovies);
    }

    @action setQueryString(value) {
        this.queryString = value;
    }

    @action setGenres(genres) {
        this.filters.with_genres = genres;
        console.log(genres);
    }


    @action pageIncrement() {
        this.page++;
    }

    @action resetStore() {
        this.currentPage = 0;
        this.movies = undefined;
    }

    async fetchMovies(pageToFetch = 1) {
        console.log('fetching movies page ', pageToFetch);
        const params = this._getDefinedFilters();
        params.page = pageToFetch;
        try {
            this.setIsLoading(true);
            const response = await axios.get("http://localhost:3200/films", {
                params,
                withCredentials: true
            });
            console.log(response);
            if (response.data.success === true) {
                const { page, total_pages, results } = response.data.movies;
                if (pageToFetch === 1) {
                    this.setMovies(results);
                } else {
                    this.pushMovies(results)
                }
                this.setPage(page);
                this.setTotalPages(total_pages);
                // console.log(response.data.movies.results);
            } else {
                this.setMovies(null);
                console.log(response.data.error);
            }
        } catch (e) {
            this.setMovies(null);
            console.log(e);
        } finally {
            this.setIsLoading(false);
        }
    }

    async fetchSearchResults(pageToFetch = 1) {
        console.log('searching for', this.queryString);
        this.setIsLoading(true);
        try {
            const response = await axios.get("http://localhost:3200/films", {
                params: { query: this.queryString, page: pageToFetch },
                withCredentials: true
            });
            if (response.data.success === true) {
                const { page, total_pages, results } = response.data.movies;
                if (pageToFetch === 1) {
                    console.log('setting movies', results);
                    this.setMovies(results);
                } else {
                    console.log('pushing movies');
                    this.pushMovies(results)
                }
                this.setPage(page);
                this.setTotalPages(total_pages);
            } else {
                this.setMovies(null);
                console.log(response.data.error);
            }
        } catch (e) {
            console.error(e);
        } finally {
            this.setIsLoading(false);
        }
    }

    _getDefinedFilters() {
        const currentFilters = toJS(this.filters);
        const definedFilterKeys = [];
        Object.keys(this.filters).forEach(filter => {
            if (this.filters[filter] !== undefined) {
                definedFilterKeys.push(filter);
            }
        });

        let filters = {};
        if (definedFilterKeys.length > 0) {
            definedFilterKeys.forEach(filter => {
                filters[filter] = currentFilters[filter];
            });
        }

        return toJS(filters);
    }
}

export default new LibraryStore();