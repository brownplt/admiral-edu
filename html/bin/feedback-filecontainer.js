"use strict";

// contains js code extracted from feedback-file-container.html

function save(json, callback){
}

// FIXME same function appears in feedback-filecontainer.js
// FIXME installing top-level functions called "load" and "save"
//  probably not such a great idea.
function load(callback){
  if (maybeFileUrl) {
    $.get(maybeFileUrl,
          function(contentStr,type1) {
            $.post(commentsUrl,
                   function(comments,type2) {
                     callback(comments,contentStr);
                   },
                   "json");
          },
          "text");
  }
}
