import * as twgl from 'twgl.js';


const vertexShader = `
// a_position describes the canvas.
attribute vec2 a_position;

// a_texCoord describes a unit square
attribute vec2 a_texCoord;

// uniform to pass canvas resolution
uniform vec2 u_resolution;

// variable to pass next to the fragment shader, will contain
// a_texCoord to properly interpolate the texture at given point
varying vec2 v_texCoord;

void main() {
   // convert the rectangle from pixels to 0.0 to 1.0
   vec2 zeroToOne = a_position / u_resolution;

   // convert from 0->1 to 0->2
   vec2 zeroToTwo = zeroToOne * 2.0;

   // convert from 0->2 to -1->+1 (clipspace)
   vec2 clipSpace = zeroToTwo - 1.0;

   // Flips the y-axis
   gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);

   // pass the texCoord to the fragment shader
   // The GPU will interpolate this value between points.
   v_texCoord = a_texCoord;
}
`;

const fragmentShader = `
precision mediump float;

// our 3 canvases
uniform sampler2D u_edges;
uniform sampler2D u_edgeLabels;
uniform sampler2D u_nodes;
uniform sampler2D u_labels;
uniform sampler2D u_hovers;
uniform sampler2D u_hoverNodes;
uniform sampler2D u_mouse;

// the texCoords passed in from the vertex shader.
// note: we're only using 1 set of texCoords which means
//   we're assuming the canvases are the same size.
varying vec2 v_texCoord;

// This is a helper function: return color from_, but when it's
// transparent, return to_.
vec4 or_transparent(vec4 from_, vec4 to_) {
     if(from_.a == 0.0) {  // is transparent, use to_
         return to_;
     } else {
         return from_;
     }
}

void main() {
     // Return overlay in this order: mouse, hoverNodes, hovers,
     // labels, nodes, edgeLabels, edges.
     // Ordering here should be the reverse of <canvas> layers on the
     // sigmajs side.
     vec4 tmp_color = texture2D(u_mouse, v_texCoord);  // Look up a pixel from edges canvas.
     tmp_color      = or_transparent(tmp_color, texture2D(u_hoverNodes, v_texCoord));
     tmp_color      = or_transparent(tmp_color, texture2D(u_hovers, v_texCoord));
     tmp_color      = or_transparent(tmp_color, texture2D(u_labels, v_texCoord));
     tmp_color      = or_transparent(tmp_color, texture2D(u_nodes, v_texCoord));
     tmp_color      = or_transparent(tmp_color, texture2D(u_edgeLabels, v_texCoord));
     tmp_color      = or_transparent(tmp_color, texture2D(u_edges, v_texCoord));

     gl_FragColor = tmp_color;
}
`;

function debugSnapshot(data) {
  // let consoleS = `font-size: 1px;
  //   line-height: ${gl.canvas.height}px;
  //   padding: ${gl.canvas.height * .5}px ${gl.canvas.width * .5}px;
  //   background-size: ${gl.canvas.width}px ${gl.canvas.height}px;
  //   background-image: url(${data});`
  // console.log('image:', data);
  // console.log('consoleS', consoleS);
  // console.log('%c', consoleS);

  let imgId = 'debug-image';
  let image = new Image();
  if(document.getElementById(imgId)) {
    image = document.getElementById(imgId);
  } else {
    image.setAttribute('id', imgId);
    document.getElementsByTagName('body')[0].appendChild(image);
  }
  image.src = data;

  // (function(url) {
  //   var image = new Image();
  //   image.onload = function() {
  //     console.log('%c', [
  //       'font-size: 1px;',
  //       `line-height: ${this.height}px;`,
  //       'padding: ' + this.height * .5 + 'px ' + this.width * .5 + 'px;',
  //       'background-size: ' + this.width + 'px ' + this.height + 'px;',
  //       'background-image: url(' + url + ');'
  //     ].join(' '));
  //   };
  //   image.src = url;
  // })(data);

}

export function takeScreenshot(sigma) {
  let c = sigma.container;
  let edges      = c.getElementsByClassName('sigma-edges')[0];
  let edgeLabels = c.getElementsByClassName('sigma-edgeLabels')[0];
  let nodes      = c.getElementsByClassName('sigma-nodes')[0];
  let labels     = c.getElementsByClassName('sigma-labels')[0];
  let hovers     = c.getElementsByClassName('sigma-hovers')[0];
  let hoverNodes = c.getElementsByClassName('sigma-hoverNodes')[0];
  let mouse      = c.getElementsByClassName('sigma-mouse')[0];

  // temporary canvas element onto which we will draw
  let tmp = document.createElement('canvas');
  let tmpId = 'tmp-screenshot';
  if(document.getElementById(tmpId)) {
    tmp = document.getElementById(tmpId);
  } else {
    tmp.setAttribute('id', tmpId);
    c.prepend(tmp);
  }
  for(let k in edges.style) { tmp.style[k] = edges.style[k] };
  tmp.setAttribute('width', edges.getAttribute('width'));
  tmp.setAttribute('height', edges.getAttribute('height'));

  let gl = twgl.getContext(tmp);

  const programInfo = twgl.createProgramInfo(gl, [vertexShader, fragmentShader]);

  const arrays = {
    a_position: { numComponents: 2,
                  data: [0, 0,
                         gl.canvas.width, 0,
                         0, gl.canvas.height,

                         0, gl.canvas.height,
                         gl.canvas.width, 0,
                         gl.canvas.width, gl.canvas.height] },
    a_texCoord: { numComponents: 2,
                  data: [0.0, 0.0,
                         1.0, 0.0,
                         0.0, 1.0,

                         0.0, 1.0,
                         1.0, 0.0,
                         1.0, 1.0] }
  };
  const bufferInfo = twgl.createBufferInfoFromArrays(gl, arrays);

  twgl.resizeCanvasToDisplaySize(gl.canvas);
  //gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);

  let texParams = function(src) {
    return { //wrapS: gl.CLAMP_TO_EDGE,
             //wrapT: gl.CLAMP_TO_EDGE,
             //min: gl.NEAREST,
             //mag: gl.NEAREST,
             src };
    };
  const uniforms = {
    u_edges      : twgl.createTexture(gl, texParams(edges)),
    u_edgeLabels : twgl.createTexture(gl, texParams(edgeLabels)),
    u_nodes      : twgl.createTexture(gl, texParams(nodes)),
    u_labels     : twgl.createTexture(gl, texParams(labels)),
    u_hovers     : twgl.createTexture(gl, texParams(hovers)),
    u_hoverNodes : twgl.createTexture(gl, texParams(hoverNodes)),
    u_mouse      : twgl.createTexture(gl, texParams(mouse)),
    u_resolution : [gl.canvas.width, gl.canvas.height]
  };

  gl.useProgram(programInfo.program);
  twgl.setBuffersAndAttributes(gl, programInfo, bufferInfo);
  twgl.setUniformsAndBindTextures(programInfo, uniforms);
  twgl.drawBufferInfo(gl, bufferInfo);

  ret = gl.canvas.toDataURL('image/png');

  // NOTE: Comment this to see the grapshot on your canvas.
  c.removeChild(tmp);

  // debugSnapshot(ret);

  // console.log('programInfo', programInfo);
  // console.log('bufferInfo', bufferInfo);
  // console.log('edges', edges);
  // console.log('nodes', nodes);
  // console.log('labels', labels);
  // console.log('uniforms', uniforms);

  // throw new Error('debugging');

  return ret;
}



// DEPRECATED


const vertexShader1 = `
attribute vec2 a_position;
attribute vec2 a_texCoord;

uniform vec2 u_resolution;

varying vec2 v_texCoord;

void main() {
   // convert the rectangle from pixels to 0.0 to 1.0
   vec2 zeroToOne = a_position / u_resolution;

   // convert from 0->1 to 0->2
   vec2 zeroToTwo = zeroToOne * 2.0;

   // convert from 0->2 to -1->+1 (clipspace)
   vec2 clipSpace = zeroToTwo - 1.0;

   gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);

   // pass the texCoord to the fragment shader
   // The GPU will interpolate this value between points.
   v_texCoord = a_texCoord;
}
`;

const fragmentShader1 = `
precision mediump float;

    // our 3 canvases
    uniform sampler2D u_nodes;
    uniform sampler2D u_edges;
    uniform sampler2D u_labels;

    // the texCoords passed in from the vertex shader.
    // note: we're only using 1 set of texCoords which means
    //   we're assuming the canvases are the same size.
    varying vec2 v_texCoord;

    void main() {
         vec3 white = vec3(1);

         // Look up a pixel from nodes canvas
         vec4 n_color = texture2D(u_nodes, v_texCoord);

         // Look up a pixel from edges canvas
         vec4 e_color = texture2D(u_edges, v_texCoord);

         // Look up a pixel from labels canvas
         vec4 l_color = texture2D(u_labels, v_texCoord);

         // return overlay of l_color on n_color on e_color
         vec4 tmp_color = n_color;

         // if(tmp_color.a > 0.5) {  // l_color is transparent (i.e. no text here), use n_color
         //   tmp_color = e_color;
         // }

//         if(all(equal(tmp_color.rgb, white))) {  // l_color is white, use n_color
//           tmp_color = n_color;
//         }
//         if(all(equal(tmp_color.rgb, white))) {  // n_color is white, use e_color
//           tmp_color = e_color;
//         }
//         if(all(equal(tmp_color.rgb, white))) {  // e_color is white, set transparent
//           //tmp_color = vec4(tmp_color.rgb, 1);
//         }

         // gl_FragColor = e_color;  // returns edges
         gl_FragColor = l_color;  // returns labels
         // gl_FragColor = n_color;  // returns empty

//         if(all(equal(n_color.rgb, white))) {  // n_color is white, use e_color
//             gl_FragColor = e_color;
//         } else {
//             if(all(equal(e_color.rgb, white))) {  // e_color is white, set transparent
//                 gl_FragColor = vec4(e_color.rgb, 1);
//             } else {
//                 gl_FragColor = n_color;
//             }
//         }
    }
`;



function setupTexture(gl, canvas, textureUnit, program, uniformName) {
  let tex = gl.createTexture();

  updateTextureFromCanvas(gl, tex, canvas, textureUnit);

  // Set the parameters so we can render any size image.
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

  let location = gl.getUniformLocation(program, uniformName);
  gl.uniform1i(location, textureUnit);
}

function updateTextureFromCanvas(gl, tex, canvas, textureUnit) {
  gl.activeTexture(gl.TEXTURE0 + textureUnit);
  gl.bindTexture(gl.TEXTURE_2D, tex);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, canvas);
}


export function takeScreenshot1(sigma) {
  // https://stackoverflow.com/questions/12590685/blend-two-canvases-onto-one-with-webgl
  let c = sigma.container;
  let edges = c.getElementsByClassName('sigma-edges')[0];
  let nodes = c.getElementsByClassName('sigma-nodes')[0];
  let labels = c.getElementsByClassName('sigma-labels')[0];
  // let sceneCtx = scene.getContext('2d');
  // sceneCtx.globalAlpha = 1;
  // sceneCtx.drawImage(edges, 0, 0);
  // return scene.toDataURL('image/png');
  let edgesCtx = twgl.getContext(edges);
  //edgesCtx.globalAlpha = 1;
  //edgesCtx.drawImage(nodes, 0, 0);

  // temporary canvas element onto which we will draw
  let tmp = document.createElement('canvas');
  tmp.setAttribute('id', 'tmp-screenshot');
  for(let k in edges.style) { tmp.style[k] = edges.style[k] };
  tmp.width = edges.width;
  tmp.height = edges.height;
  c.appendChild(tmp);
  let gl = twgl.getContext(tmp);

  const program = twgl.createProgramFromSources(gl, [vertexShader1, fragmentShader1]);

  gl.useProgram(program);

  const positionLocation = gl.getAttribLocation(program, "a_position");
  const texCoordLocation = gl.getAttribLocation(program, "a_texCoord");

  // Create a buffer for the position of the rectangle corners.
  // a_position
  let buffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.enableVertexAttribArray(positionLocation);
  gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
  // Set a rectangle the same size as the image.
  setRectangle(gl, 0, 0, gl.canvas.width, gl.canvas.height);

  // a_texCoord buffer
  const texCoordBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
      0.0,  0.0,
      1.0,  0.0,
      0.0,  1.0,
      0.0,  1.0,
      1.0,  0.0,
      1.0,  1.0]), gl.STATIC_DRAW);
  gl.enableVertexAttribArray(texCoordLocation);
  gl.vertexAttribPointer(texCoordLocation, 2, gl.FLOAT, false, 0, 0);

  // lookup uniforms
  let resolutionLocation = gl.getUniformLocation(program, "u_resolution");

  // set the resolution
  gl.uniform2f(resolutionLocation, gl.canvas.width, gl.canvas.height);

  let tex1 = setupTexture(gl, nodes, 0, program, "u_nodes");
  let tex2 = setupTexture(gl, edges, 1, program, "u_edges");
  let tex3 = setupTexture(gl, labels, 2, program, "u_labels");

  // Draw the rectangle.
  gl.drawArrays(gl.TRIANGLES, 0, 6);

  ret = gl.canvas.toDataURL('image/png');

  c.removeChild(tmp);

  debugSnapshot(ret);

  throw new Error('debugging1');

  //return ret;
}


function setRectangle(gl, x, y, width, height) {
  const x1 = x;
  const x2 = x + width;
  const y1 = y;
  const y2 = y + height;
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
    x1, y1,
    x2, y1,
    x1, y2,
    x1, y2,
    x2, y1,
    x2, y2]), gl.STATIC_DRAW);
}
