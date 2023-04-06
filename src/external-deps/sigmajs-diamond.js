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
import TriangleProgram from "./sigmajs-triangle-abstract";

const POINTS = 6;
const ATTRIBUTES = 5;

const ANGLE_1_1 = (0 * Math.PI) / 4;
const ANGLE_1_2 = (2 * Math.PI) / 4;
const ANGLE_1_3 = (4 * Math.PI) / 4;
const ANGLE_2_1 = (4 * Math.PI) / 4;
const ANGLE_2_2 = (6 * Math.PI) / 4;
const ANGLE_2_3 = (8 * Math.PI) / 4;

export default class NodeProgram extends TriangleProgram {
  constructor(gl) {
    super(gl, POINTS, ATTRIBUTES);
  }

  triangleDefinitions(data) {
    const size = data.size / 1.7;  // experimental...

    return [ { x: data.x, y: data.y, size: size, color: data.color, angles: [ANGLE_1_1, ANGLE_1_2, ANGLE_1_3] },
             { x: data.x, y: data.y, size: size, color: data.color, angles: [ANGLE_2_1, ANGLE_2_2, ANGLE_2_3] } ];
  }
}
