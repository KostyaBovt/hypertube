import { observable, action } from "mobx";

class AuthStore {
    @observable fields = {
        first_name: '',
        last_name: '',
        username: '',
        email: '',
        password: '',
        confirm_password: ''
    }

    @observable errors = {
        first_name: '',
        last_name: '',
        username: '',
        email: '',
        password: '',
        confirm_password: ''
    }

    @action setError(fieldName, error) {
        this.errors[fieldName] = error;
    }

    @action resetStore() {
        this.fields = {
            first_name: '',
            last_name: '',
            username: '',
            email: '',
            password: '',
            confirm_password: ''
        };

        this.errors = {
            first_name: '',
            last_name: '',
            username: '',
            email: '',
            password: '',
            confirm_password: ''
        };
    }

    validate() {

    }
}

export default new AuthStore();
