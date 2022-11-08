// Based on sigma.js/src/rendering/webgl/programs/node.fast.ts

import { NodeDisplayData } from "sigma/types";
import { floatColor } from "sigma/utils";
import vertexShaderSource from "sigma/rendering/webgl/shaders/node.fast.vert.glsl";
import fragmentShaderSource from "sigma/rendering/webgl/shaders/node.fast.frag.glsl";
import { AbstractNodeProgram } from "sigma/rendering/webgl/programs/common/node";
import { RenderParams } from "sigma/rendering/webgl/programs/common/program";
import CircleNodeProgram from 'sigma/rendering/webgl/programs/node.fast';

const POINTS = 2;
const ATTRIBUTES = 4;

/*
export default class NodeContourFastProgram extends AbstractNodeProgram {
  //constructor(gl : WebGLRenderingContext) {
  constructor(gl) {
    super(gl, vertexShaderSource, fragmentShaderSource, POINTS, ATTRIBUTES);
    this.bind();
    }
*/

export default class NodeContourFastProgram extends CircleNodeProgram {
  //process(data: NodeDisplayData, hidden: boolean, offset: number): void {
  process(data, hidden, offset) {
    const array = this.array;
    let i = offset * POINTS * ATTRIBUTES;

    if (hidden) {
      // contour
      array[i++] = 0;
      array[i++] = 0;
      array[i++] = 0;
      array[i++] = 0;

      // circle
      array[i++] = 0;
      array[i++] = 0;
      array[i++] = 0;
      array[i++] = 0;

      return;
    }

    const color = floatColor(data.color);
    const black = floatColor('black');

    // contour
    array[i++] = data.x;
    array[i++] = data.y;
    array[i++] = data.size + 1;
    array[i++] = black;

    // circle
    array[i++] = data.x;
    array[i++] = data.y;
    array[i++] = data.size;
    array[i] = color;
}

  //render(params: RenderParams): void {
  render(params) {
    if (this.hasNothingToRender()) return;

    const gl = this.gl;

    const program = this.program;
    gl.useProgram(program);

    gl.uniform1f(this.ratioLocation, 1 / Math.sqrt(params.ratio));
    gl.uniform1f(this.scaleLocation, params.scalingRatio);
    gl.uniformMatrix3fv(this.matrixLocation, false, params.matrix);

    gl.drawArrays(gl.POINTS, 0, this.array.length / ATTRIBUTES);
  }
}
