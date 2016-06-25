haste-standalone
================
Create zero configuration web apps with Haste.App that don't require a web
server.

**Note that haste-standalone has been merged into
[Haste.App](http://github.com/valderman/haste-app) -- unless you are using
Haste version 0.5.x or older, you should not be using this repository!**

Installation
------------

`cabal install /path/to/cloned/haste-standalone`

Usage
-----
Build your Haste.App application as usual, but replace `runApp my_config`
with `runStandaloneApp`; compile with Haste and GHC to obtain your binary and
JS.

Now run your binary with the `--embed` option, followed by the name of the
JavaScript file produced by Haste and any accompanying static files:

    $ ./my_app --embed my_app.js foo.html bar.jpg ...

If you're on Windows, this will fail spectacularly. If you're on Linux or OSX,
your binary should now contain all the files you specified. To check that
everything worked, use the `--list-files` option to list all embedded files:

    $ ./my_app --list-files
    *my_app.js
    foo.html
    bar.jpg

Note the asterisk next to `my_app.js`. This indicates that `my_app.js` is
considered to be the main app client program. This is important, since the
main program will be served with host and port settings for Haste.App.

Now you can launch your web application simply by executing it. On start,
it will print the host and port on which your application is served.
For more information and configuration options, run any binary built with
`haste-standalone` with the `--help` option.
