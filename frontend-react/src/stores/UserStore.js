import { observable, action } from "mobx";
import axios from 'axios';

class UserStore {
    @observable user = undefined;

    @action setUser(data) {
        this.user = data;
    }

    @action forgetUser() {
        this.user = null;
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
}

export default new UserStore();
