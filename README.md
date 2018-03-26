Published as the [billstclair/elm-localstorage package](http://package.elm-lang.org/packages/billstclair/elm-localstorage/latest) at elm-lang.org

I've built many projects that use JavaScript's `localStorage` mechanism for persistent storage. I've waited patiently for Evan to add a mechanism to the standard library, but he refuses to settle on an API. So I'm doing it.

Some ideas here are cribbed from Paul Statezny's [knledg/elm-local-storage-ports](https://github.com/knledg/elm-local-storage-ports), but his package is not in the Elm repository. This one also provides an API-compatible pure Elm version that you can use from `elm reactor` during development (with only session persistence, but that suffices for development).

See the [example](https://github.com/billstclair/elm-localstorage/tree/master/example) directory for use of both simulated ports, which work in `elm reactor`, and real ports.

To use the real ports with your own application, create the five ports, `getItem`, `setItem`, `clear`, `listKeys`, and `receiveItem`, as is done in [example/PortExample.elm](https://github.com/billstclair/elm-localstorage/tree/master/example/PortExample.elm). Copy the [site](https://github.com/billstclair/elm-localstorage/tree/master/site) directory, inluding its `js` subdirectory. In `index.html`, change the `<title>`, and change `PortExample` to the name of your top-level application module. If you want to use different names for the ports, you can change those here as well.

Finally, compile your application Elm file into `site/index.js`:

    cd ...
    elm make MyApplication.elm --output site/index.js
    
Happy hacking!

Bill St. Clair<br/>
24 March, 2018

