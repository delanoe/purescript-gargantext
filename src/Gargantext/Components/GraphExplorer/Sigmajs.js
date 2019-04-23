'use strict';

var dummyClass = 'DummyClass';
exports.edgeShapesClass = dummyClass;
exports.filterClass = dummyClass;
exports.forceAtlas2Class = dummyClass;
exports.loadGEXFClass = dummyClass;
exports.loadJSONClass = dummyClass;
exports.nOverlapClass = dummyClass;
exports.neoCypherClass = dummyClass;
exports.neoGraphItemsProducersClass = dummyClass;
exports.nodeShapesClass = dummyClass;
exports.randomizeNodePositionsClass = dummyClass;
exports.relativeSizeClass = dummyClass;
exports.sigmaClass = dummyClass;
exports.sigmaEnableSVGClass = dummyClass;
exports.sigmaEnableWebGLClass = dummyClass;
exports.forceLinkClass = dummyClass;

if (typeof window !== 'undefined') {

const SJS = require('react-sigma');
const FL = require('react-sigma/lib/ForceLink');

exports.edgeShapesClass = SJS.EdgeShapes;
exports.filterClass = SJS.Filter;
exports.forceAtlas2Class = SJS.ForceAtlas2;
exports.loadGEXFClass = SJS.LoadGEXF;
exports.loadJSONClass = SJS.LoadJSON;
exports.nOverlapClass = SJS.NOverlap;
exports.neoCypherClass = SJS.NeoCypher;
exports.neoGraphItemsProducersClass = SJS.NeoGraphItemsProducers;
exports.nodeShapesClass = SJS.NodeShapes;
exports.randomizeNodePositionsClass = SJS.RandomizeNodePositions;
exports.relativeSizeClass = SJS.RelativeSize;
exports.sigmaClass = SJS.Sigma;
exports.sigmaEnableSVGClass = SJS.SigmaEnableSVG;
exports.sigmaEnableWebGLClass = SJS.SigmaEnableWebGL;
exports.forceLinkClass = FL.default;

}

exports.setSigmaRef = function(props) {
  if (props && props.sigma) {
    window.sigmaGargInstance = props.sigma;
  }
};
exports.getSigmaRef = function() {
  return window.sigmaGargInstance;
};
exports.goToImpl = function(cam) {
  return function(props) {
    console.log("goTo", cam, props);
    return cam.goTo(props);
  };
};
exports.pauseForceAtlas2 = function() {
  var s = window.sigmaGargInstance;
  if (s) {
    if (s.isForceAtlas2Running()) {
      s.stopForceAtlas2()
    }
    else {
      s.startForceAtlas2()
    }
  }
};

exports.cursor_size = 10;

// TODO
//exports.shift_key = false;
exports.shift_key = true;

exports.trackMouse = function(e) {

    if(!exports.shift_key) {
      var partialGraph = window.sigmaGargInstance;
      console.log('FUN t.minimap:trackMouse');
      // new sigma.js 2D mouse context
      var ctx = partialGraph.renderers[0].contexts.mouse;
      ctx.globalCompositeOperation = "source-over";

      // clear zone each time to prevent repeated frame artifacts
      ctx.clearRect(50, 50,
                    partialGraph.renderers[0].container.offsetWidth,
                    partialGraph.renderers[0].container.offsetHeight);

      // classic mousemove event or other similar non-sigma events

      var coord = window.sigma.utils.mouseCoords(e)
      var x = (coord.x + coord.clientX) / 2 // ; // sigma.utils.getX(e);
      var y = (coord.y + coord.clientY) /2 // ; // sigma.utils.getY(e);
      console.log(coord);
      // optional: make more labels appear on circle hover (/!\ costly /!\ esp. on large graphs)
//      if (partialGraph.conf.moreLabelsUnderArea) {
//        // convert screen => mouse => cam
//        var mouseCoords = (50,50); // sigma.utils.mouseCoords(e)
//        var camCoords = partialGraph.cam.cameraPosition(mouseCoords.x, mouseCoords.y)
//
//        var exactNodeset = circleGetAreaNodes(camCoords.x,camCoords.y)
//        // console.log("nodes under circle:", exactNodeset)
//
//        // we'll use labelThreshold / 3 as the "boosted" cam:size threshold
//        var pfx = partialGraph.cam.readPrefix
//        var toRedraw = []
//        for (var k in exactNodeset) {
//          var n = partialGraph.graph.nodes(exactNodeset[k])
//          if(!n.hidden && n[pfx+'size'] > (partialGraph.customSettings.labelThreshold / 3)) {
//            toRedraw.push(n)
//          }
//        }
//        redrawNodesInHoverLayer(toRedraw, "hovers")
//      }

      // draw the circle itself
      ctx.strokeStyle = '#000';
      ctx.lineWidth = 1;
      ctx.fillStyle = "#71C3FF";
      ctx.globalAlpha = 0.5;
      ctx.beginPath();
      ctx.arc(x, y, 30.0, 0, Math.PI * 2, true);
      //ctx.arc(x, y, partialGraph.gui.circleSize, 0, Math.PI * 2, true);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();
      ctx.globalAlpha = 1

    }

};

exports.sigmaOnMouseMove = function(e) {
    return function() {
      console.log('sigmaOnMouseMove');
      if(typeof(window.sigmaGargInstance) !== "undefined") {
          if(exports.cursor_size>0) exports.trackMouse(e);
      }
    };
};
