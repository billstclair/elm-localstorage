Published as the [billstclair/elm-localstorage](http://package.elm-lang.org/packages/billstclair/elm-localstorage/latest) package at elm-lang.org

This module is built on top of the [billstclair/elm-port-funnel](http://package.elm-lang.org/packages/billstclair/elm-localstorage/latest) package, so it can share two ports with other port funnel modules. It requires Elm 0.19.

I've built many projects that use JavaScript's `localStorage` mechanism for persistent storage. Enough to know the features I need. This package makes that experience available to the community.

Some ideas here are cribbed from Paul Statezny's [knledg/elm-local-storage-ports](https://github.com/knledg/elm-local-storage-ports), but his package is not in the Elm repository. This one also provides an API-compatible pure Elm version that you can use it from `elm reactor` during development (with only session persistence, but that suffices for development).

See the [example](https://github.com/billstclair/elm-localstorage/tree/master/example) directory for use of both simulated ports, which work in `elm reactor`, and real ports.

The example is live at [lisplog.org/localstorage](https://lisplog.org/localstorage/).

To use the real ports with your own application, create the two ports, `cmdPort` and `subPort`, as is done in [example/Main.elm](https://github.com/billstclair/elm-localstorage/tree/master/example/Main.elm). Copy the [site](https://github.com/billstclair/elm-localstorage/tree/master/site) directory, inluding its `js` subdirectory. In `index.html`, change the `<title>`, and, if necessary change `Main` to the name of your top-level application module. If you want to use different names for the ports, you can change those here as well.

Finally, compile your application Elm file into `site/elm.js`:

    cd ...
    elm make Main.elm --output site/elm.js
    
Happy hacking!

Bill St. Clair<br/>
24 March, 2018

