This directory contains an example of using the `LocalStorage` module.

There are two top-level application files:

1. ReactorExample.elm uses simulated ports to enable testing the code in elm reactor.

 To start it:

    cd .../elm-localstorage/example
    elm reactor

 Then aim your browser at http://localhost:8000/ReactorExample.elm

2. PortExample.elm uses real ports, via `site/index.html` and `site/js/localStoragePorts.js`.

 You can build `site/index.js`, which is required by `site/index.html`, with:

    cd .../elm-localstorage/example
    bin/build
