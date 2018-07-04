import React, { Component } from 'react';
import Title from '@/components/title';
import Banner from '@/components/banner';
import Navbar from '@/components/navbar';

class About extends Component {
  render() {
    return (<div>
      <Title type='about' />
      <Banner />
      <Navbar />
    </div>);
  }
};

export default About;
