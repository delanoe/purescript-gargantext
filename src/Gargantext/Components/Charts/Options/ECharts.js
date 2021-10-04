"use strict";

var ReactEcharts = require("echarts-for-react");

exports.eChartsClass = ReactEcharts.default;

/**
 * @XXX "echarts-for-react" unsuitable to proper PureScript implementation
 *       regarding event listeners
 * @name listenerFn1
 * @param {function} fn
 * @returns
 */
exports.listenerFn1 = function(fn) {
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
exports.dispatchAction = function(eChartsInstance) {
  return function(opts) {
    return function() {
      eChartsInstance.dispatchAction(opts);
    }
  }
}
