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
        console.log(this.fields[name]);
    }

    @action setError(fieldName, error) {
        console.log('setting errors', fieldName, error);
        this.errors[fieldName] = error;
    }

    register() {
        if (this._validateFields()) {
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
}

export default new AuthStore();
