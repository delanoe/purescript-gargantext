'use strict';

exports.modalShow = function(name) {
    return function(){
        var myModal = document.getElementById(name);
        var myModalInstance = new Modal(myModal);
        myModalInstance.show();
    };
};

exports.modalHide = function(name){
    return function() {
        var myModal = document.getElementById(name);
        var myModalInstance = new Modal(myModal);
        myModalInstance.hide();
    };
};
