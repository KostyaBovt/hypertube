import { observable, action } from "mobx";
import axios from 'axios';

class UserStore {
    @observable self = undefined;
    
    @action setSelf(data) {
        this.self = data;
    }

    @action forgetSelf() {
        this.self = null;
    }

    async pullSelf() {
        try {
            const response = await axios.get('http://localhost:8080/api/profile', { withCredentials: true })
            this.setSelf(response.data.payload);
            console.log(response);
        } catch (e) {
            this.self = null;
            console.error(e);
        }
    }

}

export default new UserStore();
