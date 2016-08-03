"use strict";

// contains js code extracted from browse-file-container.html

function save(json, callback){
}

function load(callback){
  if (maybeFileUrl) {
    $.get(maybeFileUrl,
          // FIXME should be checking type?
          // FIXME does setting "text" mime type ensure
          //  string or undefined as result?
          function(data,type) {
            callback({ "comments" : {} },data);
          },
          "text")
  }
}
