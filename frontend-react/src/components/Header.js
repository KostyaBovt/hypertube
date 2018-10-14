import React, { Component } from 'react';

class Header extends Component {
	render() {
		return (
			<div>
			<nav>
  				<div className="nav-wrapper">
				    <a href="" className="brand-logo" id="logo">HyperTube</a>
				    <ul id="nav-mobile" className="right hide-on-med-and-down">
				      <li><a href="">sass</a></li>
				      <li><a href="">sass <span className="new badge">4</span></a></li>
				      <li><a href="">sass</a></li>
				    </ul>
				  </div>
				</nav>	
			</div>
		);
	}
};


export default Header;
