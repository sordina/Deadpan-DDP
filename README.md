# Deadpan-DDP

![Deadpan Baby](http://i.imgur.com/Nc5mA2j.png)

A Haskell
[DDP](https://github.com/meteor/meteor/blob/devel/packages/ddp/DDP.md)
Client.

This can be used for writing clients for [Meteor](https://www.meteor.com/) sites, among other purposes.

# NOTE: The Library is currently unusable.

Things implemented so far:

* EJSON data types and conversion functions
* Connect to server
* Respond to PING
* Print all incomming EJSON server messages

## Usage

This is intended to be used in two forms. A library, and a debugging tool executable.

### Using the library

In order to use Deadpan as a library you will need to write a Deadpan monad.

This could look something like the following:

    myDeadpanApp = do
      subscribe "kittens"
      message "kittens.add" (ejobject [("cute", ejbool True)])
      sessionId <- getSessionId
      liftIO $ print sessionId


You can then run your instance as follows:

    runDeadpan "websockets://testapp.meteor.com:3000/websocket" myDeadpanApp


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

> deadpan websocets://testapp.meteor.com:3000/websocket

This will dump all server messages to STDOUT.

For further instructions on how to use the tool, you can run:

> deadpan --help


## Installing

The latest source is available [on Github](https://github.com/sordina/Deadpan-DDP).
This can be installed via the cabal tool.

> cabal install

It is intended that this package will be uploaded to Hackage at some point.
When this is completed, you will be able to install Deadpan with the following command:

> cabal install deadpan-ddp


## Testing

A test-suite can be run by calling `cabal test`.

This triggers the QuickCheck tests, as well as running some integration tests against
a demo Meteor app found under the `test/` directory.


## Binaries

Pre-compiled binaries can be found for the `deadpan` debugging tool below:

* <http://sordina.binaries.s3.amazonaws.com/deadpan-0.1.0.1-MacOSX-10.9.5-13F34.zip>


## TODO

* Write definitions for all stubs in DDP module
* Move modules under some existing namespace
* Haddock documentation
* Upload to Hackage
* Add --help flag
* Update references to test.meteor.com in the docs to point to some kind of real app
* Write test-suite
* Fix TODO notes in code
* Use more qualified imports, including for internal imports
