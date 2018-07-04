import React, { Component } from 'react';
import Hypertorus from '@/components/gl/hypertorus';

import './banner.scss';

class Banner extends Component {

  constructor(props) {
    super(props);
    this.state = {
      width: 1000,
      height: 100,
    };
  }

  updateBannerSize() {
    this.setState({
      width: this.refs.banner.clientWidth,
      height: this.refs.banner.clientHeight,
    });
  }

  componentDidMount() {
    this.updateBannerSize();
    window.addEventListener('resize', this.updateBannerSize.bind(this));
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.updateBannerSize.bind(this));
  }

  render() {
    return (<div className='banner' ref='banner'>
      <Hypertorus width={this.state.width} height={this.state.height} />
    </div>);
  }

};

export default Banner;
