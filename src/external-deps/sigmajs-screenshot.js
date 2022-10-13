import * as twgl from 'twgl.js';


const vertexShader = `
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

const fragmentShader = `
precision mediump float;

    // our 2 canvases
    uniform sampler2D u_canvas1;
    uniform sampler2D u_canvas2;

    // the texCoords passed in from the vertex shader.
    // note: we're only using 1 set of texCoords which means
    //   we're assuming the canvases are the same size.
    varying vec2 v_texCoord;

    void main() {
         // Look up a pixel from first canvas
         vec4 color1 = texture2D(u_canvas1, v_texCoord);

         // Look up a pixel from second canvas
         vec4 color2 = texture2D(u_canvas2, v_texCoord);

         // return the 2 colors multiplied
         // TODO Modify this
         gl_FragColor = color1 * color2;
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


export function takeScreenshot(sigma) {
  // https://stackoverflow.com/questions/12590685/blend-two-canvases-onto-one-with-webgl
  let c = sigma.container;
  let edges = c.getElementsByClassName('sigma-edges')[0];
  let nodes = c.getElementsByClassName('sigma-nodes')[0];
  // let sceneCtx = scene.getContext('2d');
  // sceneCtx.globalAlpha = 1;
  // sceneCtx.drawImage(edges, 0, 0);
  // return scene.toDataURL('image/png');
  let edgesCtx = twgl.getContext(edges);
  //edgesCtx.globalAlpha = 1;
  //edgesCtx.drawImage(nodes, 0, 0);

  let gl = edgesCtx;  // TODO Create separate canvas for this

  const program = twgl.createProgramFromSources(gl, [vertexShader, fragmentShader]);

  gl.useProgram(program);

  const positionLocation = gl.getAttribLocation(program, "a_position");
  const texCoordLocation = gl.getAttribLocation(program, "a_texCoord");

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

  // Create a buffer for the position of the rectangle corners.
  let buffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.enableVertexAttribArray(positionLocation);
  gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

  // Set a rectangle the same size as the image.
  setRectangle(gl, 0, 0, gl.canvas.width, gl.canvas.height);

  let tex1 = setupTexture(gl, nodes, 0, program, "u_canvas1");
  let tex2 = setupTexture(gl, edges, 1, program, "u_canvas2");

  // Draw the rectangle.
  gl.drawArrays(gl.TRIANGLES, 0, 6);

  return gl.canvas.toDataURL('image/png');
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
