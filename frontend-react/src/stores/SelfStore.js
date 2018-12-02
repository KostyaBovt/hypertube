import { observable, action, reaction } from "mobx";
import axios from 'axios';
import LibraryStore from "./LibraryStore";
import i18n from "../helpers/i18n";

class SelfStore {
    @observable self = undefined;

    @action setSelf(data) {
        this.self = data;
    }

    @action updateSelfField(fieldName, value) {
        this.self[fieldName] = value;
    }

    @action forgetSelf() {
        this.self = null;
    }

    async pullSelf() {
        try {
            const response = await axios.get('http://localhost:8080/api/profile', { withCredentials: true });
            this.setSelf(response.data.payload);
            i18n.changeLanguage(this.self.locale);
            reaction(() => this.self.locale,
                     locale => i18n.changeLanguage(locale));
        } catch (e) {
            this.setSelf(null);
            console.error(e);
        }
    }

    async importPictureFromSocial() {
        try {
            const response = await axios.get('http://localhost:8080/api/social_avatar', { withCredentials: true });
            if (response.data.status === "ok") {
                const avatar = response.data.payload.avatar;
                this.updateSelfField('avatar', avatar);
            } else {
                const error = Object.values(response.data.reason)[0];
                console.error(error);
            }
            console.log(response);
        } catch (e) {
            console.error(e);
        }
    }
    
    async updateProfile(fieldName, value) {
        const data = {};
        data[fieldName] = value;
        
        try {
            const response = await axios.post('http://localhost:8080/api/profile', data, { withCredentials: true });
            if (response.data.status === "ok") {
                const updatedValue = response.data.payload[fieldName];
                this.updateSelfField(fieldName, updatedValue);
                return { success: true };
            } else {
                const error = Object.values(response.data.reason)[0];
                return { success: false, error }
            }
        } catch (e) {
            console.error(e);
            return { success: false, error: 'Unexpected error occured' };
        }
    }
    
    async updateEmail(value) {
        const data = { email: value };
        try {
            const response = await axios.post('http://localhost:8080/api/profile/email', data, { withCredentials: true });
            console.log(response);
            if (response.data.status === "ok") {
                return { success: true };
            } else {
                const error = Object.values(response.data.reason)[0];
                return { success: false, error }
            }
        } catch (e) {
            console.error(e);
            return { success: false, error: 'Unexpected error occured' };
        }
    }

    async updateLocale(value) {
        const data = { locale: value };
        try {
            const response = await axios.post('http://localhost:8080/api/profile/locale', data, { withCredentials: true });
            if (response.data.status === "ok") {
                this.updateSelfField('locale', value);
                LibraryStore.resetMovies()
                LibraryStore.setSearchMode(false);
            } else {
                // const error = Object.values(response.data.reason)[0];
                return false;
            }
        } catch (e) {
            console.error(e);
            return false;
        }
    }

    async updatePassword(old_password, new_password) {
        const data = { old_password, new_password };
        try {
            const response = await axios.post('http://localhost:8080/api/profile/pass', data, { withCredentials: true });
            console.log(response);
            if (response.data.status === "ok") {
                return { success: true };
            } else {
                return { success: false, error: response.data.reason };
            }
        } catch (e) {
            console.error(e);
            return { success: false, error: { old_password: 'Some unexpected error occured' }}
        }
    }
    
}

export default new SelfStore();
