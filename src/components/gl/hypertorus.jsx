import React, { Component } from 'react';
import {
  Color3,
  Vector3,
  Vector4,
  MeshBuilder,
  VertexData,
  StandardMaterial,
  Mesh,
  Engine,
  Scene,
  ArcRotateCamera,
  HemisphericLight,
} from 'babylonjs';

const DOUBLE_PI = 2 * Math.PI;
const THIRD_PI = Math.PI / 3;

function lerp(min, max, t) {
  t = Math.min(1, Math.max(0, t));
  return (1 - t) * min + t * max;
}

function calcColor(angle) {
  const a = Math.abs(angle) % DOUBLE_PI;
  const s = Math.floor(a / THIRD_PI) % 6;
  const t = Math.min(1, a % THIRD_PI);
  switch (s) {
    case 0:
      return { r: 1, g: t, b: 0 };
    case 1:
      return { r: 1 - t, g: 1, b: 0 };
    case 2:
      return { r: 0, g: 1, b: t };
    case 3:
      return { r: 0, g: 1 - t, b: 1 };
    case 4:
      return { r: t, g: 0, b: 1 };
    case 5:
      return { r: 1, g: 0, b: 1 - t };
    default:
      return { r: 0, g: 0, b: 0 };
  }
}

function project4to3(vec) {
  var w = vec.w + 2;
  return new Vector3(vec.x / w, vec.y / w, vec.z / w);
}

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
    return { x, y, z };
  }

  addHypertorus(scene) {
    const umin = 0;
    const umax = 2 * Math.PI;
    const vmin = 0;
    const vmax = 2 * Math.PI;
    const nu = 32;
    const nv = 64;

    for (let i = 0; i < nu; i += 1) {
      if (i & (3 > 1)) continue;

      const params = [];
      const positions = [];
      const colors = [];
      for (let j = 0; j <= nv; j += 1) {
        const v = lerp(vmin, vmax, j / nv);

        for (let k = 0; k < 2; k += 1) {
          const u = lerp(umin, umax, (i + k) / nu) + v / 4;

          // point in 2d parametric space
          params.push(u, v);

          // positions in 4d and 3d
          var p4 = new Vector4(
            Math.cos(u),
            Math.sin(u),
            Math.cos(v),
            Math.sin(v),
          );
          var p3 = project4to3(p4);
          positions.push(p3.x, p3.y, p3.z);

          // colors
          var c = calcColor(u);
          colors.push(c.r, c.g, c.b, 1.0);
        }
      }

      const indices = [];
      for (let j = 0; j + 2 < params.length / 2; j += 1) {
        indices.push(j, j + 1, j + 2);
      }

      const mesh = new Mesh('mesh' + i, scene);
      const material = new StandardMaterial();
      material.backFaceCulling = false;
      mesh.material = material;

      const vertexData = new VertexData();
      vertexData.indices = indices;
      vertexData.positions = positions;
      vertexData.colors = colors;
      vertexData.applyToMesh(mesh, true);
    }
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
    camera.attachControl(engine.getRenderingCanvas());

    const light = new HemisphericLight('light', new Vector3(1, 1, 1), scene);

    this.addAxis(scene);
    this.addHypertorus(scene);
    return { scene, camera };
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
