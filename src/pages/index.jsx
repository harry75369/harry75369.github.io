import React, { Component } from 'react';
import Title from '@/components/title';
import Banner from '@/components/banner';
import Navbar from '@/components/navbar';
import Posts from '@/components/posts';

class Index extends Component {
  render() {
    return (
      <div>
        <Title type="index" />
        <Banner />
        <Navbar />
        <Posts />
      </div>
    );
  }
}

export default Index;
