var ElmLocalStoragePorts = function() {};

ElmLocalStoragePorts.prototype.subscribe =
  function(app, getPortName, setPortName, clearPortName, responsePortName, listKeysPortName) {

    if (!getPortName) getPortName = "getItem";
    if (!setPortName) setPortName = "setItem";
    if (!clearPortName) clearPortName = "clear";
    if (!listKeysPortName) listKeysPortName = "listKeys";
    if (!responsePortName) responsePortName = "response";

    if (app.ports[responsePortName]) {

      var responsePort = app.ports[responsePortName];

      if (app.ports[getPortName]) {
        app.ports[getPortName].subscribe(function(key) {
          var val = null;
          try {
            val = JSON.parse(localStorage.getItem(key))
          } catch (e) {}
          responsePort.send({
            key: key,
            value: val
          })
        });
      } else {
        console.warn("The " + getPortName + " port is not connected.");
      }

      if (app.ports[setPortName]) {
        app.ports[setPortName].subscribe(function(kv) {
          var key = kv[0];
          var json = kv[1];
          if (json === null) {
            localStorage.removeItem(key);
          } else {
            localStorage.setItem(key, JSON.stringify(json));
          }
        });
      } else {
        console.warn("The " + setPortName + " port is not connected.");
      }

      if (app.ports[clearPortName]) {
        app.ports[clearPortName].subscribe(function(prefix) {
          if (prefix) {
            var cnt = localStorage.length;
            for (var i = cnt - 1; i >= 0; --i) {
              var key = localStorage.key(i);
              if (key && key.startsWith(prefix)) {
                localStorage.removeItem(key);
              }
            }
          } else {
            localStorage.clear();
          }
        });
      } else {
        console.warn("The " + clearPortName + " port is not connected.");
      }

      if (app.ports[listKeysPortName]) {
        app.ports[listKeysPortName].subscribe(function(prefix) {
          var cnt = localStorage.length;
          var keys = [];
          for (var i = 0; i < cnt; i++) {
            var key = localStorage.key(i);
            if (key && key.startsWith(prefix)) {
              keys.push(key);
            }
          }
          responsePort.send(keys);
        });
      } else {
        console.warn("The " + listKeysPortName + " port is not connected.");
      }
    } else {
      console.warn("The " + responsePortName + " port is not connected.");
    }
  };

module.exports.ElmLocalStoragePorts = ElmLocalStoragePorts;
