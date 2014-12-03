% Deadpan-DDP
% Lyndon Maydwell
% December 04, 2014

# Deadpan-DDP

## On GitHub

![...](http://i.imgur.com/Nc5mA2j.png)

<https://github.com/sordina/Deadpan-DDP#deadpan-ddp> 

## On Hackage

![...](http://i.imgur.com/Nc5mA2j.png)

<https://hackage.haskell.org/package/Deadpan-DDP> 

## Binary Downloads

![...](http://i.imgur.com/Nc5mA2j.png)

<http://sordina.binaries.s3.amazonaws.com/deadpan-0.3.0.1-MacOSX-10.9.5-13F34.zip>

# Deadpan

## What is Deadpan?

Deadpan is a Haskell DDP client.

## Why do I care?

Becuase it lets you talk to DDP servers!

# DDP

## What is DDP?

[DDP](https://github.com/meteor/meteor/blob/devel/packages/ddp/DDP.md)
is the protocol that [Meteor.js](http://meteor.com) applications speak.

It is a protocol that speaks EJSON over Websockets asynchronously.

Subscriptions are made and retired using this protocol.

## Why do I care?

People have written Meteor applications...

You may wish to interact with them from your Haskell programs.

# EJSON

## What is EJSON?

EJSON is just JSON with a special way of interpreting specially constructed objects.

## Why do I care?

EJSON makes it easy to represent

* Dates
* Binary Data
* User-defined types

## Definition

		data EJsonValue
			= EJObject !(Data.HashMap.Strict.HashMap Text EJsonValue)
			| EJArray  !(Data.Vector.Vector EJsonValue)
			| EJString !Text
			| EJNumber !Scientific
			| EJBool   !Bool
			| EJDate   !EpochTime
			| EJBinary !ByteString
			| EJUser   !Text !EJsonValue
			| EJNull deriving (Eq)

## Notice

This is very similar to Aeson's "Value" type.

## Value Definition

		-- Defined in ‘aeson-0.7.0.6:Data.Aeson.Types.Internal’

		data Value
			= Object !Object
			| Array !Array
			| String !Text
			| Number !Scientific
			| Bool !Bool
			| Null

## Bijection

We can go back and forward between JSON and EJSON:

		ejson2value :: EJsonValue -> Value
		ejson2value (EJObject h    ) = Object (Data.HashMap.Strict.map ejson2value h)
		ejson2value (EJArray  v    ) = Array  (Data.Vector.map ejson2value v)
		ejson2value (EJString t    ) = String t
		...

		value2EJson :: Value -> EJsonValue
		value2EJson (Object o) = escapeObject o
		value2EJson (Array  a) = EJArray $ Data.Vector.map value2EJson a
		value2EJson (String s) = EJString s
		...

## No Maybes

Failing to parse EJSON just returns whatever the literal JSON would be.


# Websockets

## What are websockets?

They are a persistant, bi-directional message conduit between client and server.

Very similar to a regular socket - Thus the name.

## Why do I care?

A persistant connection means that for longer lived applications, there is less
of a sense of reconstructing temporal flow out of stateless requests. This isn't
always the best way to do things, but for some situations it can make sense.

You can envision a websocket based application as a conversation, rather than
a sequence of request/responses.

DDP uses websockets for its communication mechenism.

## Asynchronous

An important side-effect is that the server may send data to the client
without waiting for a request, or a response Res1 corresponding to a
request Req1 my be sent after requests Req2, Rec3, Rec4.


# The Deadpan Datatype

## `DeadpanApp`?

DeadpanApp is a monadic DSL that makes the high-level actions of a DDP application
available to users through several conveniently defined functions.

## RPC

    clientRPCMethod :: Text
                    -> Maybe [EJsonValue]
                    -> Text
                    -> Maybe Text
                    -> DeadpanApp ()

## Subscription

    subscribe :: Text
              -> Text
              -> Maybe [ EJsonValue ]
              -> DeadpanApp ()


# Stateful Behavior

## Where we at?

Since a DDP app is inherently communicating in a stateful context,
`DeadpanApp` is actually just a thin wrapper over `StateT IO`.

The applications state is defined as `AppState`:

    data AppState cb = AppState
      { _defaultCallback :: cb
      , _callbackSet     :: Lookup cb
      , _collections     :: TVar EJsonValue
      }

With the cb parameter being set to... `DeadpanApp`.


# Running DeadpanApp

## How do I run my App?

`Web.DDP.Deadpan` provides functions for running your application:

    runClient :: AppState Callback
              -> Params
              -> DeadpanApp a
              -> IO a

Would be the most common.

# Example

## Show me more!

    {-# LANGUAGE OverloadedStrings #-}
    module RPCClient where
    import Web.DDP.Deadpan

    main = go $ getURI "http://localhost:3000/websocket"

    go (Left  err   ) = print err
    go (Right params) = loggingClient >>= \clnt -> runClient clnt params app

    app = do void $ liftIO getLine
             clientRPCMethod "realMethod"    Nothing "testid1" Nothing
             void $ liftIO getLine
             clientRPCMethod "missingMethod" Nothing "testid2" Nothing
             void $ liftIO getLine

# TODO

## What's left

* Synchronous wrappers around async actions
* Useful defaults for data callbacks
* Make state sharable with TVars
* Bugfixes
