import React, { Component } from 'react';
import * as BL from 'babylonjs';

class Hypertorus extends Component {

  buildScene(scene) {
    const light1 = new BL.HemisphericLight("light1", new BL.Vector3(1, 1, 0), scene);
    const light2 = new BL.PointLight("light2", new BL.Vector3(0, 1, -1), scene);
    const sphere = BL.MeshBuilder.CreateSphere("sphere", {}, scene);
  }

  componentDidMount() {
    const canvas = this.refs.canvas;
    const engine = new BL.Engine(canvas);

    const scene = new BL.Scene(engine);
    scene.clearColor = BL.Color3.White();

    const camera = new BL.ArcRotateCamera("Camera", Math.PI / 2, Math.PI / 2, 2, BL.Vector3.Zero(), scene);
    camera.attachControl(canvas, true);

    this.babylon = { engine, scene, camera };
    this.buildScene(scene);
    this.babylon.engine.runRenderLoop(() => {
      scene.render();
    });
  }

  render() {
    return <canvas ref='canvas' width={this.props.width} height={this.props.height}></canvas>;
  }
};

export default Hypertorus;
