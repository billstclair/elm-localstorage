This directory contains an example of using the `LocalStorage` module.

You can run the example with simulated local storage with:

    git clone git@github.com:billstclair/elm-localstorage.git
    cd elm-localstorage/example
    elm reactor

Then aim your browser at http://localhost:8000/Main.elm

You can build `site/elm.js`, which is required by `site/index.html`, with:

    cd .../elm-localstorage/example
    bin/build

Then aim your browser at file:///.../elm-localstorage/example/site/index.html

Or run `elm reactor` as above, and aim your browser at http://localhost:8000/site/index.html
