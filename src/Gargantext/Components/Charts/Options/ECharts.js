"use strict";

import * as ReactEcharts from "echarts-for-react";

let eChartsClass = ReactEcharts.default;
export { eChartsClass };

/**
 * @XXX "echarts-for-react" unsuitable to proper PureScript implementation
 *       regarding event listeners
 * @name listenerFn1
 * @param {function} fn
 * @returns
 */
export function listenerFn1(fn) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    fn(args[0])()
  }
};
/**
 * @link https://echarts.apache.org/en/api.html#echartsInstance.dispatchAction
 * @name dispatchAction
 * @param {object} eChartsInstance instanceof ECharts
 * @param {object} opts
 * @returns
 */
export function dispatchAction(eChartsInstance) {
  return function(opts) {
    return function() {
      eChartsInstance.dispatchAction(opts);
    }
  }
}
