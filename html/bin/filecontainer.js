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
        
  var xhr = new XMLHttpRequest();
  xhr.open("POST", loadURL, true);
  xhr.setRequestHeader('Content-Type', 'application/json; charset=UTF-8');
  xhr.send();
  xhr.onreadystatechange = function () {
    if (xhr.readyState == 4) {
      callback(xhr.responseText);
    };
  }; 
}
