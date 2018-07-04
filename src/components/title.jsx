import React, { Component } from 'react';
import Helmet from 'react-helmet';

class Title extends Component {
  getTitle() {
    const pageTitles = {
      'index': '首页',
      'about': '关于',
      '404': '没找到哦',
    };
    if (this.props.type && this.props.type in pageTitles) {
      return pageTitles[this.props.type];
    }
    if (this.props.name) {
      return this.props.name;
    }
    return 'Unknown Page';
  }

  render() {
    return (<div>
      <Helmet title={`Chaoya Li - ${this.getTitle()}`} />
    </div>);
  }
};

export default Title;
