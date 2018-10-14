import { observable, action } from "mobx";

class AuthStore {
    @observable.box fields = {
        first_name: '',
        last_name: '',
        username: '',
        email: '',
        password: '',
        confirm_password: ''
    }

    @observable.box errors = {
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

    resetStore() {
        this.fields.set({
            first_name: '',
            last_name: '',
            username: '',
            email: '',
            password: '',
            confirm_password: ''
        });
        this.errors.set({
            first_name: '',
            last_name: '',
            username: '',
            email: '',
            password: '',
            confirm_password: ''
        })
    }

    validate() {

    }
}

export default new AuthStore();
