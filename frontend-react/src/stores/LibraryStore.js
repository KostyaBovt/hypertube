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
        if (response.data.success === true){
            this.movies = response.data.movies.results;
            console.log(response.data.movies.results);
        }
        else {
            console.log(response.data.error);
        }
    }

}

export default new LibraryStore();