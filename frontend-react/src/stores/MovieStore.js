import { observable, action } from "mobx";
import axios from 'axios';

class MovieStore {
    @observable isLoading = false;
    @observable movie = undefined;
    @observable stream = undefined;
    @observable comments = [];

    @action setMovie(movie) {
        this.movie = movie;
    }

    @action resetMovie() {
        this.movie = undefined;
    }
    @action setComments(comments) {
        this.comments = comments;
    }

    @action addComment(comment) {
        this.comments.unshift(comment);
    }

    @action setStream(stream) {
        this.stream = stream;
    }

    async fetchMovieDetails(movieId) {
        const url = `http://localhost:3200/film_details/${movieId}`;
        try {
            const response = await axios.get(url, {
                withCredentials: true
            });
            if (response.data.success) {
                const { movie_details: details, streaming } = response.data;
                this.setMovie({ ...details, streaming });
                console.log(details, streaming);
            } else {
                this.setMovie(null);
            }
        } catch (e) {
            console.error(e);
            this.setMovie(null);
        }
    }

    async fetchComments(movieId) {
        const url = `http://localhost:8080/api/comments`;
        try {
            const response = await axios.get(url, {
                withCredentials: true,
                params: {
                    id: movieId,
                }
            });
            if (response.data.status === 'ok') {
                this.setComments(response.data.payload);
                console.log(response.data.payload);
            }
        } catch (e) {
            console.error(e);
        }
    }

    async postComment(movieId, text) { 
        const url = `http://localhost:8080/api/comments`;
        const body = {imdb_id: movieId, text: text};

        try {
            const response = await axios.post(url, body, {
                withCredentials: true,
            });
            if (response.data.status === 'ok') {
                console.log('added comment', response.data);
                this.addComment(response.data.payload);
            }
        } catch (e) {
            console.error(e);
        }
    }
}

export default new MovieStore();