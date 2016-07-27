"use strict";

// contains js code extracted from browse-file-container.html

function save(json, callback){
}

function load(callback){
  if (maybeFileUrl) {
    $.get(maybeFileUrl,
          function(data,type) {
            callback("{ \"comments\" : {} }",data);
          },
          "text")
  }
}
