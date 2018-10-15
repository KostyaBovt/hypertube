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

    @observable fields_login = {
        login: '',
        password: '',

    }

    @observable errors_login = {
        login: '',
        password: '',
    }

    @action setFieldValue(name, value) {
        this.fields[name] = value;
        console.log(this.fields[name]);
    }

    @action setError(fieldName, error) {
        console.log('setting errors', fieldName, error);
        this.errors[fieldName] = error;
    }

    @action setFieldValueLogin(name, value) {
        this.fields_login[name] = value;
        console.log(this.fields_login[name]);
    }

    @action setErrorLogin(fieldName, error) {
        console.log('setting errors', fieldName, error);
        this.errors_login[fieldName] = error;
    }

    register() {
        if (this._validateFields()) {
            // Submit user info to the server
        }
    }

     login() {
        if (this._validateFieldsLogin()) {
            // Submit user info to the server

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
        const { password, confirmPassword } = this.fields;

        if (password !== confirmPassword) {
            this.setError("confirmPassword", "Passwords doesn't match.");
            isValid = false;
        }

        return isValid;
    }

    _validateFieldsLogin() {
        let isValid = true;
        const { login, password } = this.fields_login;
        if (login < 6){
             this.setErrorLogin("login", "Login is too short");
             isValid = false;
        }
        else if (login > 16) {
            this.setErrorLogin("login", "Login is too long");
            isValid = false;
        }
        else if (password < 6) {
            this.setErrorLogin("password", "Password is too short");
            isValid = false;
        }
        else if (password > 128) {
            this.setErrorLogin("login", "Password is too long");
            isValid = false;
        }

        return isValid;
    }


}

export default new AuthStore();
