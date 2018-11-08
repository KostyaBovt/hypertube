import { observable, action } from "mobx";
import * as EmailValidator from 'email-validator';
import axios from 'axios';

import UserStore from './SelfStore';

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

    @action setErrors(errors) {
        Object.keys(errors).forEach(field => {
            this.errors[field] = errors[field];
        });
    }

    async register() {
        if (this._validateFields(['fname','lname','uname','email','password','confirmPassword'])) {
            const { ...fields } = this.fields;
            this.clearErrors();
            try {

                const response = await axios.post('http://localhost:8080/api/auth/registration', fields, {withCredentials: true});
                if (response.data.status === "ok") {
                    this.resetStore();
                    return true;
                } else if (response.data.status === "error") {
                    this.setErrors(response.data.reason);
                    return false;
                }   

                console.log(response);
            } catch (e) {
                console.error(e);
            }
        }
    }

    async login() {
            const { uname, password } = this.fields;
            this.clearErrors();
            try {
                const response = await axios.post('http://localhost:8080/api/auth/login', { uname, password }, {withCredentials: true});
                if (response.data.status === 'ok') {
                    UserStore.setSelf(response.data.payload);
                } else {
                    this.setErrors({ uname: 'Invalid username or password' });
                }   
                console.log(response);
            } catch (e) {
                console.log(e);
                return false;
            }
    }

    async lostPass() {
        if (this._validateFields(['email'])) {
            const { email } = this.fields;
            this.clearErrors();
            try {
                const response = await axios.post('http://localhost:8080/api/auth/lostpass', { email }, {withCredentials: true});
                if (response.data.status === 'ok') {
                    return true;
                } else if (response.data.status === "error"){
                    this.setErrors(response.data.reason);
                    return false;
                }   
                console.log(response);
            } catch (e) {
                console.log(e);
                return false;
            }
        }
    }

     async newPass() {
        if (this._validateFields(['password', 'confirmPassword'])) {
            const { password } = this.fields;
            this.clearErrors();
            try {
                const response = await axios.post('http://localhost:8080/api/auth/lostpass/newpass', { password }, {withCredentials: true});
                if (response.data.status === 'ok') {
                    return true;
                } else if (response.data.status === "error"){
                    this.setErrors(response.data.reason);
                    return false;
                }   
                console.log(response);
            } catch (e) {
                console.log(e);
                return false;
            }
        }
    }

    async logout() {
        try {
            const response = await axios.post('http://localhost:8080/api/auth/logout', null, {withCredentials: true});
            console.log(response);
        } catch (e) {
            console.error(e);
        }
    }

    @action clearErrors() {
        this.errors = {
            fname: '',
            lname: '',
            uname: '',
            email: '',
            password: '',
            confirmPassword: ''
        };
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

    _validateFields(fields) {
        let isValid = true;
        const {
            fname,
            lname,
            uname,
            email,
            password,
            confirmPassword
        } = this.fields;

        fields.forEach(name => {
            if (name === "uname") {
                if (uname.length < 6){
                    this.setError(name, "Username is too short.");
                    isValid = false;
                } else if (uname.length > 16) {
                    this.setError(name, "Username is too long");
                    isValid = false;
                }
            } else if (name === "fname"){
                if (fname.length < 1){
                    this.setError(name, "First name is too short.");
                    isValid = false;
                } else if (fname.length > 50) {
                    this.setError(name, "First name is too long");
                    isValid = false;
                }
            } else if (name === "lname"){
                if (lname.length < 1){
                    this.setError(name, "Last name is too short.");
                    isValid = false;
                } else if (lname.length > 50) {
                    this.setError(name, "Last name is too long");
                    isValid = false;
                }
            } else if (name === "password"){
                if (password.length < 8){
                    this.setError(name, "Password is too short.");
                    isValid = false;
                } else if (password.length > 20) {
                    this.setError(name, "Password is too long");
                    isValid = false;
                }
            } else if (name === "confirmPassword") {
                if (password !== confirmPassword) {
                    this.setError("confirmPassword", "Passwords doesn't match.");
                    isValid = false;
                }
            } else if (name === "email"){
                if (EmailValidator.validate(email) === false){
                    this.setError(name, "Email is invalid.");
                    isValid = false;
                }
            }

        });

        return isValid;
    }
}

export default new AuthStore();
