# Deadpan-DDP

![Deadpan Baby](http://i.imgur.com/Nc5mA2j.png)

A Haskell
[DDP](https://github.com/meteor/meteor/blob/devel/packages/ddp/DDP.md)
Client.

This can be used for writing clients for [Meteor](https://www.meteor.com/) sites, among other purposes.

* [Other Clients](http://www.meteorpedia.com/read/DDP_Clients)

## NOTE: The Library is currently incomplete. Several important callbacks are so-far undefined.

### Things implemented so far:

#### Debugging Application `deadpan`

* EJSON data types and conversion functions
* Connect to server
* Respond to PING
* Print all incomming EJSON server messages
* Blocking style RPC

#### Library

* Write DDP clients in a monadic DSL
* Set initial callbacks
* Update callbacks at runtime
* Shared data-store

### Things missing:

* Data subscription helpers


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


#### EJson

As part of the implementation of the DDP protocol, an EJson data-format
library has been written for Haskell.

This can be found under `Data.EJson`.

This primarily allows conversion between `Data.Aeson.Value` data,
and `Data.EJson.EJsonValue` data.

Lenses, Prisms, and Aeson instances are provided for this library.


### Using the `deadpan` debugging tool

Run `deadpan` against an existing Meteor installation as follows:

> deadpan websocets://meteor.com/websocket

This will dump all server messages to STDOUT.

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

A test-suite can be run by calling `cabal test`.

This triggers the QuickCheck tests, as well as running some integration tests against
a demo Meteor app found under the `test/` directory.


## Binaries

Pre-compiled binaries can be found for the `deadpan` debugging tool below:

* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.1.0.1-MacOSX-10.9.5-13F34.zip>
* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.2.0.0-MacOSX-10.9.5-13F34.zip>
* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.2.0.1-MacOSX-10.9.5-13F34.zip>
* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.3.0.1-MacOSX-10.9.5-13F34.zip>
* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.4.0.0-MacOSX-10.9.5-13F34.zip>
* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.4.1.0-MacOSX-10.9.5-13F34.zip>


## TODO

You can look for incomplete items in the source by running `make todo`.

* Write definitions for all stubs in DDP module
* Fix error on exit "recv: invalid argument (Bad file descriptor)" check out <https://github.com/k0001/pipes-network/issues/2>
* Haddock documentation
* Write test-suite
* Fix TODO notes in code
* Use more qualified imports, including for internal imports
* Narrow package dependency versions
* Consider ditching the state monad in favor of a pure reader (conn, TVar other-stuff...)
* Random number generation as-per the spec
* Find out why the protocol isn't being accepted for debugging meteor.com
* Consider creating a blocking version of the API that auto-generates and responds to IDs
