import React, { Component } from 'react';
import Title from '@/components/title';
import Banner from '@/components/banner';
import Navbar from '@/components/navbar';

import './about.scss';

class Presentation extends Component {
  render() {
    return (
      <div class="presentation">
      </div>
    );
  }
}

class About extends Component {
  render() {
    return (
      <div>
        <Title type="about" />
        <Banner />
        <Navbar />
        <Presentation />
      </div>
    );
  }
}

export default About;
