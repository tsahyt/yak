# yak

An experimental, radically over-engineered library for handling IRC messages in
Haskell.

## Design

Every IRC message type has its own type, all hinging around the central `Msg`
type. A `Msg` is parameterized by a type level command string, and
a heterogeneous list of parameter types. Combined with a number of suitable
parameter types to encode the structure of an IRC message, this provides
a simple type level DSL for declaring IRC messages.

Messages can be `emit`ted (rendered) or `fetch`ed (parsed). The behaviour of
these functions depends on the concrete type, as determined through type
inference or explicit annotation. There is an existential wrapper `SomeMsg` for
passing around *any* IRC message.

Messages can be created using a polymorphic variadic function `build`, which
serves as a general purpose constructor for all IRC message types.

## Predefined Messages

The `Network.Yak.Client` module defines message types commonly used by IRC
clients. It implements the "living standard" found
[here](https://modern.ircdocs.horse). Responses can be found in
`Network.Yak.Responses`, and capability negotiation in
`Network.Yak.Capabilities`. This should suffice for most applications.

## Status

This library is highly experimental and the interface may turn out to be rather
cumbersome to use. There are test cases in place for common IRC messages, but
most of the library is untested
