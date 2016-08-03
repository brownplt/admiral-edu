"use strict";

// contains js code extracted from file-container.html

function save(json, callback){
  var xhr = new XMLHttpRequest();
  xhr.open("POST", saveURL, true);
  xhr.setRequestHeader('Content-Type', 'application/json; charset=UTF-8');
  xhr.send(json);
  xhr.onloadend = callback;
}

function load(callback){
  // FIXME check 'type' variable? jquery docs
  // not so great here.
  if (maybeFileUrl) {
    $.get(maybeFileUrl,
          function(contentStr,type1) {
            $.post(loadURL,
                   function(comments,type2) {
                     callback(comments,contentStr);
                   },
                   "json");
          },
          "text");
  };
}
