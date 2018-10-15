import { observable, action } from "mobx";

class UserStore {
    @observable self = undefined;
    
    pullSelf() {
        // Pull user info from server and save to this.self
    }
}

export default new UserStore();
