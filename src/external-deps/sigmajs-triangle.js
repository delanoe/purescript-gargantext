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
import vertexShaderSource from "sigma/rendering/webgl/shaders/node.vert.glsl";
import fragmentShaderSource from "sigma/rendering/webgl/shaders/node.frag.glsl";
import { AbstractProgram, RenderParams } from "sigma/rendering/webgl/programs/common/program";

const POINTS = 1;
const ATTRIBUTES = 5;

const ANGLE_1 = 0;
//const ANGLE_2 = (2 * Math.PI) / 3;
//const ANGLE_3 = (4 * Math.PI) / 3;

export default class NodeProgram extends AbstractProgram {
  // positionLocation: GLint;
  // sizeLocation: GLint;
  // colorLocation: GLint;

  // matrixLocation: WebGLUniformLocation;
  // sqrtZoomRatioLocation: WebGLUniformLocation;
  // correctionRatioLocation: WebGLUniformLocation;

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

    array[i++] = data.x;
    array[i++] = data.y;
    array[i++] = data.size;
    array[i++] = color;
    array[i] = ANGLE_1;
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
