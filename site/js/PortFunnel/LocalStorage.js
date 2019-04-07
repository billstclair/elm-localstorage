//////////////////////////////////////////////////////////////////////
//
// LocalStorage.js
// JavaScript runtime code for Elm LocalStorage module.
// Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

(function() {
  var moduleName = 'LocalStorage';
  var sub = PortFunnel.sub;

  PortFunnel.modules[moduleName].cmd = dispatcher;

  // Let the Elm code know we've started
  sub.send({ module: moduleName,
             tag: "startup",
             args : null
           });

  function dispatcher(tag, args) {
    if (tag == 'get') {
      var label = args.label
      var key = args.key;
      var val = null;
      try {
        val = JSON.parse(localStorage.getItem(key))
      } catch (e) {
      }
      return { module: moduleName,
               tag: 'got',
               args: { label: label,
                       key: key,
                       value : val
                     }
             };
    } else if (tag == 'put') {
      var key = args.key;
      var json = args.value;
      if (typeof(key) == 'string') {
        if (json === null) {
          localStorage.removeItem(key);
        } else {
          var str = JSON.stringify(json);
          if (typeof(str) == 'string') {
            localStorage.setItem(key, str);
          }
        }
      }
    } else if (tag == 'listkeys') {
      var label = args.label
      var prefix = args.prefix;
      var keys = [];
      if (typeof(prefix) == 'string') {
        var cnt = localStorage.length;
        for (var i=0; i<cnt; i++) {
          var key = localStorage.key(i);
          if (key && key.startsWith(prefix)) {
            keys.push(key);
          }
        }
      }
      return { module: moduleName,
               tag: 'keys',
               args: { label: label,
                       prefix: prefix,
                       keys: keys
                     }
             };
    } else if (tag == 'clear') {
      var prefix = args;
      if (prefix) {
        if (typeof(prefix) == 'string') {
          var cnt = localStorage.length;
          for (var i=cnt-1; i>=0; --i) {
            var key = localStorage.key(i);
            if (key && key.startsWith(prefix)) {
              localStorage.removeItem(key);
            }
          }
        }
      } else {
        localStorage.clear();
      }
    } 
  }
})();   // Execute the enclosing function
