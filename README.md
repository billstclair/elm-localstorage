# elm-localstorage

elm-localstorage provides a simple and minimal API for working with local storage.

# Setting up the javascript side and implementing the Elm ports

As packages with ports cannot be published, the types of the ports are defined
instead. It is up to the user of this package in an application to provide the
port implementations.

Example code for how to do this can be found in this Gist:

https://gist.github.com/rupertlssmith/5c4e3be17830e40d64168f390f4aea24

The javascript code can be copied from this Gist, or it is also available as an
npm package. To install the npm package do:

    > npm install @the-sett/elm-localstorage


# Origins

Originally published as [billstclair/elm-localstorage](http://package.elm-lang.org/packages/billstclair/elm-localstorage/latest) version 4.0.3. The API changed as the package evolved but I preferred the simpler presentation in version 4.x. This package was forked from 4.0.3 and trimmed down to provide a minimal and simple local storage package. Thanks to
Bill St. Clair for the original work.
