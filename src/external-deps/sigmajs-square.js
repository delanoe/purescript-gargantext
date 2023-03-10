/**
 * Sigma.js WebGL Renderer Node Program
 * =====================================
 *
 * Simple program rendering nodes as triangles.
 * It does not extend AbstractNodeProgram, which works very differently, and
 * really targets the gl.POINTS drawing methods.
 * @module
 */

import { NodeDisplayData } from "sigma/types";
import { floatColor } from "sigma/utils";
import { AbstractProgram, RenderParams } from "sigma/rendering/webgl/programs/common/program";

const vertexShaderSource = `
attribute vec2 a_position;
attribute float a_size;
attribute float a_angle;
attribute vec4 a_color;

uniform mat3 u_matrix;
uniform float u_sqrtZoomRatio;
uniform float u_correctionRatio;

varying vec4 v_color;
varying vec2 v_diffVector;
varying float v_radius;
varying float v_border;

const float bias = 255.0 / 254.0;
const float marginRatio = 1.05;

void main() {
  float size = a_size * u_correctionRatio * u_sqrtZoomRatio * 4.0;
  vec2 diffVector = size * vec2(cos(a_angle), sin(a_angle));
  vec2 position = a_position + diffVector * marginRatio;
  gl_Position = vec4(
    (u_matrix * vec3(position, 1)).xy,
    0,
    1
  );

  v_border = u_correctionRatio * u_sqrtZoomRatio * u_sqrtZoomRatio;
  v_diffVector = diffVector;
  //v_radius = size / 2.0 / marginRatio;
  v_radius = 1.0;

  v_color = a_color;
  v_color.a *= bias;
}
`;

const fragmentShaderSource = `
precision mediump float;

varying vec4 v_color;
varying vec2 v_diffVector;
varying float v_radius;
varying float v_border;

const vec4 transparent = vec4(0.0, 0.0, 0.0, 0.0);

void main(void) {
  float dist = length(v_diffVector) - v_radius;

  // Originally, a triangle is drawn. This code paints it in such a
  // way that a circle is rendered.
  //float t = 0.0;
  //if (dist > v_border) {
  //  t = 1.0;
  //} else if (dist > 0.0) {
  //  t = dist / v_border;
  //}

  //gl_FragColor = mix(v_color, transparent, t);
  gl_FragColor = v_color;
}
`;

const POINTS = 6;
const ATTRIBUTES = 5;

const ANGLE_1_1 = - (1 * Math.PI) / 4;
const ANGLE_1_2 =   (1 * Math.PI) / 4;
const ANGLE_1_3 =   (3 * Math.PI) / 4;
const ANGLE_2_1 =   (3 * Math.PI) / 4;
const ANGLE_2_2 =   (5 * Math.PI) / 4;
const ANGLE_2_3 =   (7 * Math.PI) / 4;

export default class NodeProgram extends AbstractProgram {
  constructor(gl) {
    super(gl, vertexShaderSource, fragmentShaderSource, POINTS, ATTRIBUTES);

    // Locations
    this.positionLocation = gl.getAttribLocation(this.program, "a_position");
    this.sizeLocation = gl.getAttribLocation(this.program, "a_size");
    this.colorLocation = gl.getAttribLocation(this.program, "a_color");
    this.angleLocation = gl.getAttribLocation(this.program, "a_angle");

    // Uniform Location
    const matrixLocation = gl.getUniformLocation(this.program, "u_matrix");
    if (matrixLocation === null) throw new Error("AbstractNodeProgram: error while getting matrixLocation");
    this.matrixLocation = matrixLocation;

    const sqrtZoomRatioLocation = gl.getUniformLocation(this.program, "u_sqrtZoomRatio");
    if (sqrtZoomRatioLocation === null) throw new Error("NodeProgram: error while getting sqrtZoomRatioLocation");
    this.sqrtZoomRatioLocation = sqrtZoomRatioLocation;

    const correctionRatioLocation = gl.getUniformLocation(this.program, "u_correctionRatio");
    if (correctionRatioLocation === null) throw new Error("NodeProgram: error while getting correctionRatioLocation");
    this.correctionRatioLocation = correctionRatioLocation;

    this.bind();
  }

  bind() {
    const gl = this.gl;

    gl.enableVertexAttribArray(this.positionLocation);
    gl.enableVertexAttribArray(this.sizeLocation);
    gl.enableVertexAttribArray(this.colorLocation);
    gl.enableVertexAttribArray(this.angleLocation);

    gl.vertexAttribPointer(
      this.positionLocation,
      2,
      gl.FLOAT,
      false,
      this.attributes * Float32Array.BYTES_PER_ELEMENT,
      0,
    );
    gl.vertexAttribPointer(
      this.sizeLocation,
      1,
      gl.FLOAT,
      false,
      this.attributes * Float32Array.BYTES_PER_ELEMENT,
      8
    );
    gl.vertexAttribPointer(
      this.colorLocation,
      4,
      gl.UNSIGNED_BYTE,
      true,
      this.attributes * Float32Array.BYTES_PER_ELEMENT,
      12,
    );
    gl.vertexAttribPointer(
      this.angleLocation,
      1,
      gl.FLOAT,
      false,
      this.attributes * Float32Array.BYTES_PER_ELEMENT,
      16,
    );
  }

  process(data, hidden, offset) {
    const array = this.array;
    let i = offset * POINTS * ATTRIBUTES;

    if (hidden) {
      for (let l = i + POINTS * ATTRIBUTES; i < l; i++) array[i] = 0;
      return;
    }

    const color = floatColor(data.color);
    const size = data.size / 1.7;  // experimental...

    // first triangle (upper-right)
    array[i++] = data.x;
    array[i++] = data.y;
    array[i++] = size;
    array[i++] = color;
    array[i++] = ANGLE_1_1;

    array[i++] = data.x;
    array[i++] = data.y;
    array[i++] = size;
    array[i++] = color;
    array[i++] = ANGLE_1_2;

    array[i++] = data.x;
    array[i++] = data.y;
    array[i++] = size;
    array[i++] = color;
    array[i++] = ANGLE_1_3;

    // second triangle (lower-left)
    array[i++] = data.x;
    array[i++] = data.y;
    array[i++] = size;
    array[i++] = color;
    array[i++] = ANGLE_2_1;

    array[i++] = data.x;
    array[i++] = data.y;
    array[i++] = size;
    array[i++] = color;
    array[i++] = ANGLE_2_2;

    array[i++] = data.x;
    array[i++] = data.y;
    array[i++] = size;
    array[i++] = color;
    array[i] = ANGLE_2_3;
  }

  render(params) {
    if (this.hasNothingToRender()) return;

    const gl = this.gl;
    const program = this.program;

    gl.useProgram(program);

    gl.uniformMatrix3fv(this.matrixLocation, false, params.matrix);
    gl.uniform1f(this.sqrtZoomRatioLocation, Math.sqrt(params.ratio));
    gl.uniform1f(this.correctionRatioLocation, params.correctionRatio);

    gl.drawArrays(gl.TRIANGLES, 0, this.array.length / ATTRIBUTES);
  }
}
