import { observable, action } from "mobx";
import axios from 'axios';

class UserStore {
    @observable self = undefined;
    @observable user = undefined;

    @observable profileError = null;
    @observable isErrorDisplayed = false;
    
    @action setError(error) {
        this.profileError = error;
    }

    @action setIsErrorDisplayed(status) {
        this.isErrorDisplayed = status;
    }

    @action setUser(data) {
        this.user = data;
    }

    @action setSelf(data) {
        this.self = data;
    }


    @action updateSelfField(fieldName, value) {
        this.self[fieldName] = value;
    }

    @action forgetSelf() {
        this.self = null;
    }

    async pullUser(username) {
        try {
            const response = await axios.get('http://localhost:8080/api/user/' + username, { withCredentials: true })
            this.setUser(response.data.payload);
            console.log('User: ', response);
        } catch (e) {
            this.user = null;
            console.error(e);
        }
    }

    async pullSelf() {
        try {
            const response = await axios.get('http://localhost:8080/api/profile', { withCredentials: true })
            this.setSelf(response.data.payload);
            console.log('Self: ', response);
        } catch (e) {
            this.self = null;
            console.error(e);
        }
    }

    async updateProfile(fieldName, value) {
        const data = {};
        data[fieldName] = value; 

        try {
            const response = await axios.post('http://localhost:8080/api/profile', data, { withCredentials: true });
            console.log(response);
            if (response.data.status === "ok") {
                const updatedValue = response.data.payload[fieldName];
                this.updateSelfField(fieldName, updatedValue);
            } else {
                const error = Object.values(response.data.reason)[0];
                this.setError(error);
            }
        } catch (e) {
            console.error(e);
        }
    }

}

export default new UserStore();
