import { observable, action } from "mobx";

class AuthStore {
    @observable fields = {
        fname: '',
        lname: '',
        uname: '',
        email: '',
        password: '',
        confirmPassword: ''
    }

    @observable errors = {
        fname: '',
        lname: '',
        uname: '',
        email: '',
        password: '',
        confirmPassword: ''
    }

    @action setFieldValue(name, value) {
        this.fields[name] = value;
        this.errors[name] = '';
    }

    @action setError(fieldName, error) {
        this.errors[fieldName] = error;
    }

    register() {
        if (this._validateFields()) {
            // Submit user info to the server
        }
    }

    login() {
        if (this._validateFields()) {
            // Submit user info to the server
        }
    }

    lostPass() {
        if (this._validateFields()) {
            
        }
    }

    @action resetStore() {
        this.fields = {
            fname: '',
            lname: '',
            uname: '',
            email: '',
            password: '',
            confirmPassword: ''
        };

        this.errors = {
            fname: '',
            lname: '',
            uname: '',
            email: '',
            password: '',
            confirmPassword: ''
        };
    }

    _validateFields() {
        let isValid = true;
        const {
            fname,
            lname,
            uname,
            email,
            password,
            confirmPassword
        } = this.fields;


        if (uname.length < 6){
            this.setError("uname", "Username is too short.");
            isValid = false;
        }
        if (uname.length > 16) {
            this.setError("uname", "Username is too long");
            isValid = false;
        }
        if (password.length < 6) {
            this.setError("password", "Password is too short.");
            isValid = false;
        }
        if (password > 128) {
            this.setError("password", "Password is too long.");
            isValid = false;
        }
        if (password !== confirmPassword) {
            this.setError("confirmPassword", "Passwords doesn't match.");
            isValid = false;
        }

        return isValid;
    }
}

export default new AuthStore();
