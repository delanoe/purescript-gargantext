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
  if (props.sigma) {
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
exports.shift_key = false;

exports.trackMouse = function() {
    var partialGraph = window.sigmaGargInstance;
    console.log('FUN t.minimap:trackMouse')
    if(!exports.shift_key) {
        // $.doTimeout(300,function (){
            var ctx = partialGraph._core.domElements.mouse.getContext('2d');
            ctx.globalCompositeOperation = "source-over";
            ctx.clearRect(0, 0, partialGraph._core.domElements.nodes.width, partialGraph._core.domElements.nodes.height);

            x = partialGraph._core.mousecaptor.mouseX;
            y = partialGraph._core.mousecaptor.mouseY;

            ctx.strokeStyle = '#000';
            ctx.lineWidth = 1;
            ctx.fillStyle = "#71C3FF";
            ctx.globalAlpha = 0.5;
            ctx.beginPath();

            if(partialGraph._core.mousecaptor.ratio>showLabelsIfZoom){
                for(var i in partialGraph._core.graph.nodesIndex){
                        n=partialGraph._core.graph.nodesIndex[i];
                        if(n.hidden==false){
                            distance = Math.sqrt(
                                Math.pow((x-parseInt(n.displayX)),2) +
                                Math.pow((y-parseInt(n.displayY)),2)
                                );
                            if(parseInt(distance)<=exports.cursor_size) {
                                partialGraph._core.graph.nodesIndex[i].forceLabel=true;
                            } else {
                                if(typeof(n.neighbour)!=="undefined") {
                                    if(!n.neighbour) partialGraph._core.graph.nodesIndex[i].forceLabel=false;
                                } else partialGraph._core.graph.nodesIndex[i].forceLabel=false;
                            }
                        }
                }
                if(partialGraph.forceatlas2 && partialGraph.forceatlas2.count<=1) {
                    partialGraph.draw(2,2,2);
                }
            } else {
                for(var i in partialGraph._core.graph.nodesIndex){
                    n=partialGraph._core.graph.nodesIndex[i];
                    if(!n.hidden){
                        partialGraph._core.graph.nodesIndex[i].forceLabel=false;
                        if(typeof(n.neighbour)!=="undefined") {
                            if(!n.neighbour) partialGraph._core.graph.nodesIndex[i].forceLabel=false;
                            else partialGraph._core.graph.nodesIndex[i].forceLabel=true;
                        } else partialGraph._core.graph.nodesIndex[i].forceLabel=false;
                    }
                }
                if(partialGraph.forceatlas2 && partialGraph.forceatlas2.count<=1) {
                    partialGraph.draw(2,2,2);
                }
            }
            ctx.arc(x, y, exports.cursor_size, 0, Math.PI * 2, true);
            //ctx.arc(partialGraph._core.width/2, partialGraph._core.height/2, 4, 0, 2 * Math.PI, true);/*todel*/
            ctx.closePath();
            ctx.fill();
            ctx.stroke();
        // });
    }
};

exports.sigmaOnMouseMove = function(e) {
    return function() {
      console.log('sigmaOnMouseMove');
      if(typeof(window.sigmaGargInstance) !== "undefined") {
          if(exports.cursor_size>0) exports.trackMouse();
      }
    };
};
