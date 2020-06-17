# elm-localstorage

elm-localstorage provides a simple and minimal API for working with local storage that reflects the underlying browser
API very closely.

As packages with ports cannot be published, the types of the ports are defined instead. It is up to the user of this package in an application to provide the port implementations. A suitable implementation would be:

```elm
port module Ports.LocalStoragePort exposing (clear, getItem, setItem, listKeys, response)

import LocalStorage exposing (ClearPort, GetItemPort, SetItemPort, ListKeysPort, ResponsePort)


port getItem : GetItemPort msg


port setItem : SetItemPort msg


port clear : ClearPort msg


port listKeys : ListKeysPort msg


port response : ResponsePort msg
```

To work with local storage in your Elm application, you need to subscribe to the `ResponsePort` and handle responses in your update code:

```elm

type Msg
    = LocalStorageOp Operation Key Value

subscriptions : Model -> Sub Msg
subscriptions model =
    LocalStoragePort.response <| LocalStorage.responseHandler LocalStorageOp "somePrefix"


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Output )
update msg model =
    case msg of
        LocalStorageOp op key value ->
            case op of
                GetItemOperation -> ...

                SetItemOperation -> ...

                ClearOperation -> ...

                ListKeysOperation -> ...

                ErrorOperation -> ...
```

The javascript side of the ports must also be provided by the user of this package. Here is a short javascript module that implements what is needed on the javascript side:

```
var ElmLocalStoragePorts = function() {};

ElmLocalStoragePorts.prototype.subscribe =
  function(app, getPortName, setPortName, clearPortName, receivePortName, listKeysPortName) {

    if (!getPortName) getPortName = "getItem";
    if (!setPortName) setPortName = "setItem";
    if (!clearPortName) clearPortName = "clear";
    if (!receivePortName) receivePortName = "receiveItem";
    if (!listKeysPortName) listKeysPortName = "listKeys";

    var receivePort = app.ports[receivePortName];

    app.ports[getPortName].subscribe(function(key) {
      var val = null;
      try {
        val = JSON.parse(localStorage.getItem(key))
      } catch (e) {}
      receivePort.send({
        key: key,
        value: val
      })
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

    app.ports[listKeysPortName].subscribe(function(prefix) {
      var cnt = localStorage.length;
      var keys = [];
      for (var i = 0; i < cnt; i++) {
        var key = localStorage.key(i);
        if (key && key.startsWith(prefix)) {
          keys.push(key);
        }
      }
      receivePort.send({
        prefix: prefix,
        keys: keys
      });
    });
  };

module.exports.ElmLocalStoragePorts = ElmLocalStoragePorts;
```

To attach this to an Elm application, this code is used:

```
const app = Elm.Main.init({node: document.getElementById('main')});

// Subscribe to local storage.
const localStorage = new LocalStorage();
localStorage.subscribe(app);
```

# Origins

Originally published as [billstclair/elm-localstorage](http://package.elm-lang.org/packages/billstclair/elm-localstorage/latest) version 4.0.3. The API changed as the package evolved but I preferred the simpler presentation in version 4.x. This package was forked from 4.0.3 and trimmed down to provide a minimal and simple local storage package. Thanks to
Bill St. Clair for the original work.
