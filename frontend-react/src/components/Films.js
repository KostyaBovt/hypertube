import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';

@inject('FilmsStore') @observer
class Films extends Component {
	render() {
       return null;
    }
}

export default Films;
