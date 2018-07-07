import React, { Component } from 'react';
import { Link } from 'gatsby';
import 'normalize.css';

import './navbar.scss';

class Navbar extends Component {
  render() {
    return (
      <div className="navbar" id="navbar">
        <div className="nav-list">
          <Link className="nav-link" to="/#navbar">
            <button>首页</button>
          </Link>
          <Link className="nav-link" to="/about">
            <button>关于</button>
          </Link>
        </div>
        <span className="motto">What I cannot build, I cannot understand.</span>
      </div>
    );
  }
}

export default Navbar;
