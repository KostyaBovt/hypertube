import { observable, action } from "mobx";
import axios from 'axios';

class MovieStore {
    @observable isLoading = false;
    @observable movie = undefined;

    @action setMovie(movie) {
        this.movie = movie;
        console.log(movie);
    }

    @action resetMovie() {
        this.movie = undefined;
    }

    async fetchMovie(movieId) {
        const url = `http://localhost:3200/film_details/${movieId}`;
        try {
            const response = await axios.get(url, {
                withCredentials: true
            });
            console.log(response);
            if (response.data.success) {
                const details = response.data.movie_details_1;
                const streaming = response.data.movie_details_2;
                this.setMovie({ ...details, streaming });
            } else {
                this.setMovie(null);
            }
        } catch (e) {
            console.error(e);
            this.setMovie(null);
        }
        console.log(this.movie);
    }
}

export default new MovieStore();