'use strict';
exports.goToImpl = function(cam) {
  return function(props) {
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

var trackMouse = function(cursorSize, e) {

    if(!e.shiftKey) {
      var partialGraph = window.sigmaGargInstance;
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
      console.log('trackMouse', coord);
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
      ctx.arc(x, y, cursorSize, 0, Math.PI * 2, true);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();
      ctx.globalAlpha = 1

    }

};

exports.sigmaOnMouseMove = function(props) {
  return function(e) {
    return function() {
      if(typeof(window.sigmaGargInstance) !== "undefined") {
          if(props.cursorSize > 0) trackMouse(props.cursorSize, e);
      }
    };
  };
};
