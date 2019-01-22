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

const myGoto = function(sigma){
    const camera = sigma.sigma.cameras[0];
};

exports.myGoto = myGoto;
