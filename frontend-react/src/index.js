import React from 'react';
import ReactDOM from 'react-dom';
import './HyperTube.css';
import HyperTube from './HyperTube';
import Storage from './Storage';
import * as serviceWorker from './serviceWorker';
import { BrowserRouter } from 'react-router-dom';

ReactDOM.render((
	<BrowserRouter>
		 <HyperTube storage = {new Storage()}/>
	</BrowserRouter>
), document.getElementById('root'));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();

