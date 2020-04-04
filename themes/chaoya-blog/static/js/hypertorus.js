(function(containerId) {

  var scene, camera, renderer, controls;
  var hypertorus, time;
  var body = document.getElementsByTagName('body')[0];
  var width = Math.min(700, body.clientWidth - 48);
  console.log(width);
  var height = 400;
  var radius = 3;

  function init() {
    var container = document.getElementById(containerId);
    scene = new THREE.Scene();
    camera = new THREE.PerspectiveCamera(45, width/height, 0.1, 1000);
    camera.position.x = 0;
    camera.position.y = 0;
    camera.position.z = radius;
    camera.lookAt(new THREE.Vector3(0, 0, 0));
    renderer = new THREE.WebGLRenderer();
    renderer.setPixelRatio(window.devicePixelRatio);
    renderer.setSize(width, height);
    renderer.setClearColor(new THREE.Color('white'));
    controls = new THREE.OrbitControls(camera, renderer.domElement);
    container.innerHTML = '';
    container.appendChild(renderer.domElement);

    hypertorus = addHypertorus(scene);
    time = 0;
  }

  function project4to3(vec) {
    var w = vec.w + 2;
    return new THREE.Vector3(vec.x/w, vec.y/w, vec.z/w);
  }

  function addHypertorus(scene) {
    var umin = 0, umax = 2 * Math.PI;
    var vmin = 0, vmax = 2 * Math.PI;
    var nu = 32, nv = 64;
    var geoms = [];

    function lerp(min, max, t) {
      t = Math.min(1, Math.max(0, t));
      return (1 - t) * min + t * max;
    }

    function calcColor(angle) {
      var DOUBLE_PI = 2 * Math.PI;
      var THIRD_PI = Math.PI / 3;
      var s, t;

      angle = Math.abs(angle) % DOUBLE_PI;
      s = Math.floor(angle / THIRD_PI) % 6;
      t = Math.min(1, angle % THIRD_PI);
      switch (s) {
        case 0: return { r: 1, g: t, b: 0 };
        case 1: return { r: 1-t, g: 1, b: 0};
        case 2: return { r: 0, g: 1, b: t };
        case 3: return { r: 0, g: 1-t, b: 1};
        case 4: return { r: t, g: 0, b: 1 };
        case 5: return { r: 1, g: 0, b: 1-t};
      }
      return { r: 0, g: 0, b: 0};
    }

    var material = new THREE.MeshBasicMaterial({
      vertexColors: THREE.VertexColors,
      side: THREE.DoubleSide
    });

    for (var i = 0; i < nu; i += 1)
    {
      if (i & 3 > 1) continue;

      var params = [];
      var positions = [];
      var colors = [];
      for (var j = 0; j <= nv; j += 1)
      {
        var v = lerp(vmin, vmax, j / nv);
        for (var k = 0; k < 2; k += 1)
        {
          var u = lerp(umin, umax, (i + k) / nu) + v / 4;

          // point in 2d parametric space
          params.push(u, v);

          // positions in 4d and 3d
          var p4 = new THREE.Vector4(Math.cos(u), Math.sin(u), Math.cos(v), Math.sin(v));
          var p3 = project4to3(p4);
          positions.push(p3.x, p3.y, p3.z);

          // colors
          var c = calcColor(u);
          colors.push(c.r, c.g, c.b);
        }
      }

      var indices = [];
      for (var j = 0; j < params.length/2 - 2; j += 1) {
        indices.push(j, j + 1, j + 2);
      }

      var geom = new THREE.BufferGeometry();
      geom.setIndex(indices);
      geom.addAttribute('position', new THREE.Float32BufferAttribute(positions, 3));
      geom.addAttribute('color', new THREE.Float32BufferAttribute(colors, 3));
      geom.params = params;
      geoms.push(geom);

      scene.add(new THREE.Mesh(geom, material));
    }
    return geoms;
  }

  function makeRotationMatrix(time) {
    var xwRotation = new THREE.Matrix4();
    var xzRotation = new THREE.Matrix4();
    var xyRotation = new THREE.Matrix4();
    var ywRotation = new THREE.Matrix4();
    var yzRotation = new THREE.Matrix4();
    var zwRotation = new THREE.Matrix4();
    xwRotation.set(
      1,              0,               0, 0,
      0, Math.cos(time), -Math.sin(time), 0,
      0, Math.sin(time),  Math.cos(time), 0,
      0,              0,               0, 1);
    xzRotation.set(
      1,              0, 0,               0,
      0, Math.cos(time), 0, -Math.sin(time),
      0,              0, 1,               0,
      0, Math.sin(time), 0,  Math.cos(time));
    xyRotation.set(
      1, 0,              0,               0,
      0, 1,              0,               0,
      0, 0, Math.cos(time), -Math.sin(time),
      0, 0, Math.sin(time),  Math.cos(time));
    ywRotation.set(
      Math.cos(time), 0, -Math.sin(time), 0,
                   0, 1,               0, 0,
      Math.sin(time), 0,  Math.cos(time), 0,
                   0, 0,               0, 1);
    yzRotation.set(
      Math.cos(time), 0, 0, -Math.sin(time),
                   0, 1, 0,               0,
                   0, 0, 1,               0,
      Math.sin(time), 0, 0,  Math.cos(time));
    zwRotation.set(
      Math.cos(time), -Math.sin(time), 0, 0,
      Math.sin(time),  Math.cos(time), 0, 0,
                   0,               0, 1, 0,
                   0,               0, 0, 1);
    var rot3d = zwRotation
      .multiply(ywRotation)
      .multiply(xwRotation);
    var rot4d = xzRotation
      .multiply(yzRotation)
      .multiply(xyRotation);
    return rot3d.multiply(rot4d);
  }

  function render() {
    requestAnimationFrame(render);
    renderer.render(scene, camera);

    time += 0.5;
    var m = makeRotationMatrix(time/100);
    for (var i = 0; i < hypertorus.length; i += 1) {
      var geom = hypertorus[i];
      var params = geom.params;
      var positions = geom.attributes.position.array;
      for (var j = 0; j < params.length/2; j += 1) {
        var u = params[2 * j + 0];
        var v = params[2 * j + 1];
        var p4 = new THREE.Vector4(Math.cos(u), Math.sin(u), Math.cos(v), Math.sin(v));
        var p3 = project4to3(p4.applyMatrix4(m));
        positions[3 * j + 0] = p3.x;
        positions[3 * j + 1] = p3.y;
        positions[3 * j + 2] = p3.z;
      }
      geom.attributes.position.needsUpdate = true;
    }
  }

  // detect webgl support
  try {
    var canvas = document.createElement('canvas');
    var ctx = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');
  } catch (e) {
    return;
  }

  init();
  render();

}("hypertorus"));
