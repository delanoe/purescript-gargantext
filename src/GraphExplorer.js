var ge = require('react-graph-explorer');

exports.graphExplorerComponent = ge.ReactGraphExplorer;

exports.initialFile = null;

exports.getFile = function(ev) {
    return ev.target.files[0];
};

exports.parseJSON = function(contents) {
    return function(){
        return JSON.parse(contents);
    };
};

exports.logger = function(data) {
    return function() {
        console.log(data);
    };
};

exports.setupProgress = function(fr) {
    return function(fn){
        return function() {
            fr.onprogress = function(){
                fn();
            };
            fr.onloadend = function(){
                fn();
            };
        };
    };
};
