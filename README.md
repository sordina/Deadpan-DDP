# Deadpan-DDP

![Deadpan Baby](http://i.imgur.com/Nc5mA2j.png)

A Haskell
[DDP](https://github.com/meteor/meteor/blob/devel/packages/ddp/DDP.md)
Client. [(Other Clients)](http://www.meteorpedia.com/read/DDP_Clients)

[ ![build status](https://travis-ci.org/sordina/deadpan-ddp.svg) ](https://travis-ci.org/sordina/Deadpan-DDP)

Deadpan can be used for writing clients for [Meteor](https://www.meteor.com/) sites, among other purposes.

## Implemented

### Debugging Application `deadpan`

* EJSON data types and conversion functions
* Connect to server
* Respond to PING
* Print all incomming EJSON server messages
* Send EJson formatted lines of input to server as messages (with readline support)

### Library

* Monadic client DSL
* Set of callbacks available
* Update callbacks at runtime
* Shared data-store
* Blocking RPC
* Blocking subscriptions
* Respond to data-updates from the server


## Usage

This is intended to be used in two forms. A library, and a debugging tool executable.

### Using the library

In order to use Deadpan as a library you will need to write a Deadpan monad.

This could look something like the following:

    myDeadpanApp = do
      subscribe "kittens"
      response  <- rpcWait "kittens.add" (ejobject [("cute", ejbool True)])
      case response of Right good -> liftIO $ print response
                       Left  bad  -> liftIO $ print "oops!"
      return 123

You can then run your instance as follows:

    case getURI "https://www.meteor.com/websocket"
      of Right params -> runPingClient params (logEverything >> myDeadpanApp)
         Left  error  -> print error

Further examples of applications can be found in the test/client/ directory.

There are also lower-level tools provided in `Web.DDP.Deadpan.*`.

#### Collection Support

Since Meteor sites are usually heavily focused on collections,
support for automatically aggregating subscription data is
included in the form of the `collect` app.

This adds data to the `subscription-data` key of the `collections` field of the
app-state. This is updated dynamically, but you can use a blocking subscription
call if you want to ensure that it is in a sane state before you query it.

For example:

    myDeadpanApp = do
      -- Can check for an error here if desired
      _  <- subscribeWait "songs"
      as <- getAppState
      let mySong = as ^. subscriptions
                       . _EJObjectKey "songs" . _Just
                       . _EJObjectKey "prelude"
      case mySong of Just s  -> print s
                     Nothing -> print "Could not find my song"

Run with:

    runPingClient params (collect >> myDeadpanApp)


#### EJson

As part of the implementation of the DDP protocol, an EJson data-format
library has been written for Haskell.

This can be found under `Data.EJson`.

This primarily allows conversion between `Data.Aeson.Value` data,
and `Data.EJson.EJsonValue` data.

Lenses, Prisms, and Aeson instances are provided for this library.


### Using the `deadpan` debugging tool

Run `deadpan` against an existing Meteor installation as follows:

> deadpan -v Vpre1 http://meteor.com/websocket

This will dump all server messages to STDOUT.

Lines of input from STDIN are parsed as EJSON and sent as messages to the server.

Input is editable via [Haskeline.](http://hackage.haskell.org/package/haskeline)

For further instructions on how to use the tool, you can run:

> deadpan --help


## Installing

The latest source is available [on Github](https://github.com/sordina/Deadpan-DDP).
This can be installed via the cabal tool.

> cabal install

This package is available on [Hackage](http://hackage.haskell.org/package/Deadpan-DDP),
therefore, you should be able to install it by running the following commands:

    cabal update
    cabal install deadpan-ddp


## Testing

[ ![build status](https://travis-ci.org/sordina/deadpan-ddp.svg) ](https://travis-ci.org/sordina/Deadpan-DDP)

A test-suite can be run by calling `cabal test`.

This triggers the Doctests, including QuickCheck properties.

In addition to these automated tests, there are Meteor test-applications, and Deadpan
clients in the test/ directory.


## TODO

You can look for incomplete items in the source by running `make todo`.

* Fix error on exit "recv: invalid argument (Bad file descriptor)" check out <https://github.com/k0001/pipes-network/issues/2>
* Make a newtype for IDs
* Add full minimongo capabilities for data modification
* Use more qualified imports, including for internal imports
* Narrow package dependency versions
* Random number generation as-per the spec
* Adopt a more layered aproach for internal organisation with an 'apps' layer before Deadpan.hs
* Use proper opt-parser for main module
* Try out a auth example


## Binaries

Pre-compiled binaries can be found for the `deadpan` debugging tool below:

* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.6.0.1-MacOSX-10.9.5-13F34.zip>
* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.6.0.0-Linux-3.16.7-tinycore64-x86_64.zip>
* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.6.0.0-MacOSX-10.9.5-13F34.zip>
