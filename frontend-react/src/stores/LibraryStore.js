import { observable, action } from "mobx";
import axios from 'axios';

class LibraryStore {
    @observable movies = undefined;
    @observable currentPage = 0;
    @observable filters = {
        sort_by: undefined,
        release_date: undefined,
        vote_count: undefined,
        vote_average: undefined,
        with_genres: undefined
    }

    @observable queryString = '';

    @action setPage(page) {
        this.currentPage = page;
    }

    @action setMovies(movies) {
        this.movies = movies;
    }

    @action setQueryString(value) {
        this.queryString = value;
    }

    @action setGenres(genres) {
        this.filters.with_genres = genres;
        console.log(genres);
    }

    @action pushMovies(moreMovies) {
        this.movies.push(moreMovies);
    }

    @action pageIncrement() {
        this.page++;
    }

    @action resetStore() {
        this.currentPage = 0;
        this.movies = undefined;
    }

    async fetchMovies() {
        const response = await axios.get("http://localhost:3200/films", { withCredentials: true });
        console.log('movies', response);
        if (response.data.success === true){
            this.setMovies(response.data.movies.results);
            console.log(response.data.movies.results);
        }
        else {
            console.log(response.data.error);
        }
    }

    async fetchMoviesWithFilters() {
        const definedFilters = [];

        Object.keys(this.filters).forEach(filter => {
            if (this.filters[filter] !== undefined) {
                definedFilters.push(filter);
            }
        });

        console.log(definedFilters);

        if (definedFilters.length > 0) {
            const params = {};
            definedFilters.forEach(filter => {
                params[filter] = this.filters[filter];
            });

            const response = await axios.get("http://localhost:3200/films", {
                params,
                withCredentials: true
            });
            if (response.data.success === true){
                this.setMovies(response.data.movies.results);
                console.log(response.data.movies.results);
            }
            else {
                console.log(response.data.error);
            }
        }
    }

}

export default new LibraryStore();