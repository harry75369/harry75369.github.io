import React, { Component } from 'react';
import { PropTypes } from 'prop-types';
import {
  Color3,
  Vector3,
  Matrix,
  MeshBuilder,
  VertexData,
  VertexBuffer,
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
  const s = Math.min(1, Math.max(0, t));
  return (1 - s) * min + s * max;
}

function rotMatrix(t) {
  const c = Math.cos(t);
  const s = Math.sin(t);
  const xwRot = new Matrix();
  const xzRot = new Matrix();
  const xyRot = new Matrix();
  const ywRot = new Matrix();
  const yzRot = new Matrix();
  const zwRot = new Matrix();
  xwRot.m = [1, 0, 0, 0, 0, c, -s, 0, 0, s, c, 0, 0, 0, 0, 1];
  xzRot.m = [1, 0, 0, 0, 0, c, 0, -s, 0, 0, 1, 0, 0, s, 0, c];
  xyRot.m = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, c, -s, 0, 0, s, c];
  ywRot.m = [c, 0, -s, 0, 0, 1, 0, 0, s, 0, c, 0, 0, 0, 0, 1];
  yzRot.m = [c, 0, 0, -s, 0, 1, 0, 0, 0, 0, 1, 0, s, 0, 0, c];
  zwRot.m = [c, -s, 0, 0, s, c, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];
  return xwRot
    .multiply(ywRot)
    .multiply(zwRot)
    .multiply(xyRot)
    .multiply(yzRot)
    .multiply(xzRot);
}

function calcPosition(u, v, t = 0) {
  const p = [Math.cos(u), Math.sin(u), Math.cos(v), Math.sin(v)];
  const r = rotMatrix(t);
  const q = [];
  for (let i = 0; i < 4; i += 1) {
    let c = 0;
    for (let j = 0; j < 4; j += 1) {
      c += r.m[j * 4 + i] * p[j];
    }
    q.push(c);
  }
  const [x, y, z, w] = q;
  const d = w + 2;
  return new Vector3(x / d, y / d, z / d);
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

/* eslint-disable no-unused-vars */
function addAxis(scene) {
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

function addHypertorus(scene) {
  const umin = 0;
  const umax = 2 * Math.PI;
  const vmin = 0;
  const vmax = 2 * Math.PI;
  const nu = 32;
  const nv = 32;
  const stripes = [];

  for (let i = 0; i < nu; i += 1) {
    /* eslint-disable no-continue */
    if (i % 4 > 1) continue;

    const params = [];
    const positions = [];
    const colors = [];

    for (let j = 0; j <= nv; j += 1) {
      const v = lerp(vmin, vmax, j / nv);

      for (let k = 0; k < 2; k += 1) {
        const u = lerp(umin, umax, (i + k) / nu) + v / 4;

        // point in 2d parametric space
        params.push(u, v);

        // positions
        const p = calcPosition(u, v);
        positions.push(p.x, p.y, p.z);

        // colors
        const c = calcColor(u);
        colors.push(c.r, c.g, c.b, 1.0);
      }
    }

    const indices = [];
    for (let j = 0; j + 2 < params.length / 2; j += 1) {
      indices.push(j, j + 1, j + 2);
    }

    const mesh = new Mesh(`stripe${i}`, scene);
    const material = new StandardMaterial();
    material.backFaceCulling = false;
    material.specularColor = new Color3.Black();
    mesh.material = material;

    const vertexData = new VertexData();
    vertexData.indices = indices;
    vertexData.positions = positions;
    vertexData.colors = colors;
    vertexData.applyToMesh(mesh, true);

    stripes.push({ mesh, params });
  }

  return {
    stripes,
    transform: (t) => {
      for (let i = 0; i < stripes.length; i += 1) {
        const { mesh, params } = stripes[i];
        const positions = [];
        for (let j = 0; j < params.length / 2; j += 1) {
          const u = params[2 * j + 0];
          const v = params[2 * j + 1];
          const p = calcPosition(u, v, t);
          positions.push(p.x, p.y, p.z);
          mesh.updateVerticesData(VertexBuffer.PositionKind, positions);
        }
      }
    },
  };
}

function buildScene(engine) {
  const scene = new Scene(engine);
  scene.clearColor = Color3.White();

  const camera = new ArcRotateCamera(
    'camera',
    Math.PI / 2,
    Math.PI / 2,
    4,
    Vector3.Zero(),
    scene,
  );
  camera.wheelPrecision = 50;
  // camera.attachControl(engine.getRenderingCanvas());

  const light = new HemisphericLight('light', new Vector3(0, 0, 1), scene);
  // const axis = addAxis(scene);
  const hypertorus = addHypertorus(scene);

  return {
    scene,
    camera,
    light,
    // axis,
    hypertorus,
  };
}

class Hypertorus extends Component {
  componentDidMount() {
    const engine = new Engine(this.canvas);
    const { scene, hypertorus } = buildScene(engine);
    let t = 0;

    engine.runRenderLoop(() => {
      hypertorus.transform(t);
      scene.render();
      t += 0.01;
      if (Math.floor(t * 100) % 10 === 0) {
        /* eslint-disable no-console */
        console.log(engine.getFps());
      }
    });
  }

  render() {
    const { width, height } = this.props;
    return (
      <canvas
        ref={(c) => {
          this.canvas = c;
        }}
        width={width}
        height={height}
      />
    );
  }
}

Hypertorus.propTypes = {
  width: PropTypes.number.isRequired,
  height: PropTypes.number.isRequired,
};

export default Hypertorus;
