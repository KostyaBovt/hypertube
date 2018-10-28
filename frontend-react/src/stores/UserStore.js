import { observable, action } from "mobx";
import axios from 'axios';

class UserStore {
    @observable self = undefined;
    
    pullSelf() {
        axios.get('http://localhost:8080/api/profile', {withCredentials: true})
            .then((response) => {
                console.log(response);
                this.self = response.data.payload;
            })
            .catch((error) =>{
                this.self = null;
                console.log(error);
            })

    }
}

export default new UserStore();
