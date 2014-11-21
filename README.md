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


There are also lower-level tools provided in `Web.DDP.Deadpan.Internal`.


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


## Binaries

## TODO

* Write definitions for all stubs in DDP module
* Move modules under some existing namespace
* Haddock documentation
* Upload to Hackage
* Add --help flag
