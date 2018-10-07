import { observable, runInAction } from "mobx";

class Storage {
	isAuthenticated = observable.box(undefined);
};

export default Storage;
