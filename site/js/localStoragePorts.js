//////////////////////////////////////////////////////////////////////
//
// localStoragePorts.js
// JavaScript runtime code for Elm LocalStorage module.
// Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

// The single global variable defined by this file
var localStoragePorts = {};

(function() {

localStoragePorts.subscribe = subscribe;

function subscribe(app, getPortName, setPortName, clearPortName, receivePortName) {

  var receivePort = app.ports[receivePortName];

  app.ports[getPortName].subscribe(function(key) {
    var val = null;
    try {
      val = JSON.parse(localStorage.getItem(key))
    } catch (e) {
    }
    receivePort.send([key, val])
    });

  app.ports[setPortName].subscribe(function(kv) {
    var key = kv[0];
    var json = kv[1];
    if (json === null) {
      localStorage.removeItem(key);
    } else {
      localStorage.setItem(key, JSON.stringify(json));
    }
  });

  app.ports[clearPortName].subscribe(function(prefix) {
    if (prefix) {
      var cnt = localStorage.length;
      for (var i=0; i<cnt; i++) {
        var key = localStorage.key(0);
        if (key && key.startsWith(prefix)) {
          localStorage.removeItem(key);
        }
      }
    } else {
      localStorage.clear();
    }
  });
}

})();   // Execute the enclosing function
