//////////////////////////////////////////////////////////////////////
//
// PortFunnel.js
// JavaScript runtime code for billstclair/elm-port-funnel
// Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////
//
// PortFunnel is the single global variable defined by this file.
// It is an object with a `subscribe` property, a function, called as:
//
//   PortFunnel.subscribe
//     (app, {portnames: ['cmdPort', 'subPort'],
//            modules: ['Module1', ...],
//            moduleDirectory: 'js/PortFunnel'
//           });
//
// The `ports` property is optional. If included, its value should be a
// two-element array containing the name of the `Cmd` and `Sub` ports in
// `app`. They default as specified above.
//
// The `modules` property is a list of strings, each of which should
// correspond to a JavaScript file, which implements the JS side
// of a PortFunnel-aware Elm module.
//
// The `moduleDirectory` property is a string, giving the path to the
// directory containing all the module JavaScript files. It is optional,
// and defaults to 'js/PortFunnel'.
//
// When each `module` JavaScript file is loaded.
// It should set `PortFunnel.modules['moduleName']`, as illustrated in
// `ExampleModule.js`,so that it can be hooked in to the funnelling
//  mechanism below.
//
//////////////////////////////////////////////////////////////////////

var PortFunnel = {};

(function() {

PortFunnel.subscribe = subscribe; // called by HTML file
PortFunnel.modules = {};          // modules[funnelName].cmd set by module JS.
PortFunnel.sub = null;          // set below

function subscribe(app, args) {
  if (!args) args = {};
  var portNames = args.portNames;
  if (!portNames) {
    portNames = ['cmdPort', 'subPort'];
  }
  var moduleDirectory = args.moduleDirectory;
  if (!moduleDirectory) {
    moduleDirectory = 'js/PortFunnel';
  }

  var ports = app.ports;
  var sub = ports[portNames[1]];
  PortFunnel.sub = sub;

  var cmd = ports[portNames[0]];
  cmd.subscribe(function(command) {
    var returnValue = commandDispatch(command);
    if (returnValue) sub.send(returnValue);
  });  

  var modules = args.modules;
  if (modules) {
    for (var i in modules) {
      loadModule(modules[i], moduleDirectory);
    }
  }
}

// Load moduleDirectory+'/'+moduleName+'.js'
// Expect it to set PortFunnel.modules[moduleName].cmd to
// a function of two args, tag and args.
function loadModule(moduleName, moduleDirectory) {
  PortFunnel.modules[moduleName] = {};

  var src = moduleDirectory + '/' + moduleName + '.js';
  var script = document.createElement('script');
  script.type = 'text/javascript';
  script.src = src;

  document.head.appendChild(script);
}

// command is of the form:
//    { module: 'moduleName',
//      tag: 'command name for module',
//      args: {name: value, ...}
//    }
function commandDispatch(command) {
  if (typeof(command) == 'object') {
    var moduleName = command.module;
    var module = PortFunnel.modules[moduleName];
    if (module) {
      var cmd = module.cmd;
      if (cmd) {
        var tag = command.tag;
        var args = command.args;
        return cmd(tag, args);
      }
    }
  }
}

})()
