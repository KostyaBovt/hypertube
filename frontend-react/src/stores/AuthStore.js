import { observable, action } from "mobx";
import * as EmailValidator from 'email-validator';
import axios from 'axios';

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
        if (this._validateFields(['fname','lname','uname','email','password'])) {
            const { fname, lname, uname, email, password } = this.fields;
            const body = { fname, lname, uname, email, password };
            axios.post('http://localhost:8080/api/auth/registration', body, {withCredentials: true})
                .then((response) => {
                    console.log(response);
                    if (response.data.status === "ok") {
                        // email is sent and we to need notify user about it in some form
                    }
                    else if (response.data.status === "error")
                    {
                        this.errors = response.data.payload;
                    }
                })
        }
    }

    login() {
        if (this._validateFields(['uname','password'])) {
            // Submit user info to the server
        }
    }

    lostPass() {
        if (this._validateFields(['email'])) {
            
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
                }
                else if (uname.length > 16) {
                    this.setError(name, "Username is too long");
                    isValid = false;
                }
            }
            else if (name === "fname"){
                if (fname.length < 1){
                    this.setError(name, "First name is too short.");
                    isValid = false;
                }
                else if (fname.length > 50) {
                    this.setError(name, "First name is too long");
                    isValid = false;
                }
            }
            else if (name === "lname"){
                if (lname.length < 1){
                    this.setError(name, "Last name is too short.");
                    isValid = false;
                }
                else if (lname.length > 50) {
                    this.setError(name, "Last name is too long");
                    isValid = false;
                }
            }
            else if (name === "password"){
                if (password.length < 8){
                    this.setError(name, "Password name is too short.");
                    isValid = false;
                }
                else if (password.length > 20) {
                    this.setError(name, "Password name is too long");
                    isValid = false;
                }
                else if (password !== confirmPassword) {
                    this.setError("confirmPassword", "Passwords doesn't match.");
                    isValid = false;
                }

            }
            else if (name === "email"){
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
