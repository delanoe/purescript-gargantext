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

const ANGLE_1 = - (0.5 * Math.PI) / 3;
const ANGLE_2 =   (1.5 * Math.PI) / 3;
const ANGLE_3 =   (3.5 * Math.PI) / 3;

export default class NodeProgram extends TriangleProgram {
  constructor(gl) {
    super(gl, POINTS, ATTRIBUTES);
  }

  triangleDefinitions(data) {
    const gray = floatColor('#aaa')
    const size = data.size / 2.3;  // experimental...
    const contourSize = size + 0.8;  // experimental...

    return [ { x: data.x, y: data.y, size: contourSize, color: '#aaa', angles: [ANGLE_1, ANGLE_2, ANGLE_3] },
             { x: data.x, y: data.y, size: size, color: data.color, angles: [ANGLE_1, ANGLE_2, ANGLE_3] }];
  }
}
