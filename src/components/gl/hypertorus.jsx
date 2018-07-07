import React, { Component } from 'react';
import {
  Color3,
  Vector3,
  HemisphericLight,
  MeshBuilder,
  Engine,
  Scene,
  ArcRotateCamera,
} from 'babylonjs';

class Hypertorus extends Component {
  addAxis(scene) {
    const size = 2;
    const x = MeshBuilder.CreateLines(
      'xAxis',
      {
        points: [Vector3.Zero(), new Vector3(size, 0, 0)],
      },
      scene,
    );
    x.color = Color3.Red();
    const y = MeshBuilder.CreateLines(
      'yAxis',
      {
        points: [Vector3.Zero(), new Vector3(0, size, 0)],
      },
      scene,
    );
    y.color = Color3.Green();
    const z = MeshBuilder.CreateLines(
      'zAxis',
      {
        points: [Vector3.Zero(), new Vector3(0, 0, size)],
      },
      scene,
    );
    z.color = Color3.Blue();
  }

  buildScene(engine) {
    const scene = new Scene(engine);
    scene.clearColor = Color3.White();

    const camera = new ArcRotateCamera(
      'camera',
      Math.PI / 4,
      Math.PI / 3,
      2,
      Vector3.Zero(),
      scene,
    );
    // camera.attachControl(engine.getRenderingCanvas());

    const light = new HemisphericLight('light', new Vector3(1, 1, 1), scene);
    const object = MeshBuilder.CreateBox(
      'box',
      {
        faceColors: [
          new Color3(1, 0, 0),
          new Color3(0, 1, 0),
          new Color3(0, 0, 1),
          new Color3(1, 1, 0),
          new Color3(1, 0, 1),
          new Color3(0, 1, 1),
        ],
      },
      scene,
    );
    this.addAxis(scene);
    return { scene, camera, light, object };
  }

  componentDidMount() {
    const canvas = this.refs.canvas;
    const engine = new Engine(canvas);
    const { scene, camera } = this.buildScene(engine);

    engine.runRenderLoop(() => {
      camera.alpha += 0.01;
      scene.render();
    });
  }

  componentWillUnmount() {}

  render() {
    return (
      <canvas
        ref="canvas"
        width={this.props.width}
        height={this.props.height}
      />
    );
  }
}

export default Hypertorus;
