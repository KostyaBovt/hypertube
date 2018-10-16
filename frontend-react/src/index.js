import React from 'react';
import ReactDOM from 'react-dom';
import './HyperTube.css';
import HyperTube from './HyperTube';
import * as serviceWorker from './serviceWorker';
import { Provider } from 'mobx-react';
import stores from './stores';

ReactDOM.render((
		<Provider { ...stores }>
			<HyperTube/>
		</Provider>
), document.getElementById('root'));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();

