# usocket - Universal Socket Library

Version: 0.8.3
<br/>
Licence: MIT
<br/>
Repository: [usocket/usocket](https://github.com/usocket/usocket)
<br>
See also: [awesome-cl#websockets](https://github.com/CodyReichert/awesome-cl#websockets)

*This page was possible due to the excellent [official documentation](https://common-lisp.net/project/usocket/api-docs.shtml).*

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/cl-library-docs/common-lisp-libraries/issues).*

## GETTING STARTED

For the examples below, we assume the user has access to `netcat`. A pure lisp example can be found [here](http://lispcookbook.github.io/cl-cookbook/sockets.html).

0] Firstly, we

```lisp
CL-USER> (add-package-local-nickname :u :usocket)
#<PACKAGE "COMMON-LISP-USER">
```

### Example 1 - TCP and Netcat

1] On a terminal, we start a TCP server using netcat

```sh
$ nc -lv localhost 8001
Listening on [localhost] (family 0, port 8001)
```

On the lisp side, we connect to the server

```lisp
CL-USER> (defparameter socket (u:socket-connect "localhost" 8001))
SOCKET
```

```sh
$ nc -lv localhost 8001
Listening on [localhost] (family 0, port 8001)
Connection from localhost 56888 received!
```

2] Once the connection is established, we send some data from lisp to the netcat server:

```lisp
CL-USER> (defparameter str (u:socket-stream socket))
STR
CL-USER> (defun write-and-flush (string)
           (write-string string str)
           (force-output str))
WRITE-AND-FLUSH
CL-USER> (write-and-flush "hello")
NIL
```

That should cause `hello` appear on netcat.

3] We also do the reverse: type `world` on the terminal and press `Ctrl+D` to flush it.

On the lisp side,

```lisp
CL-USER> (loop :while (listen str) :do (write-char (read-char str)))
world
NIL
```

### Example 2 - Netcat and UDP

The process is similar in this case; however, instead of reading and writing to the
stream, one uses [socket-send](#socket-send) and [socket-receive](#socket-receive). The first step is the same, except we start netcat as `nc -luv`.

2] Once the connection is established, we send some data from lisp to the netcat server:

```lisp
CL-USER> (u:socket-send socket "hello" nil)
NIL
```

That should cause `hello` appear on netcat.

3] We also do the reverse: type `world` on the terminal and press `Ctrl+D` to flush it.

On the lisp side,

```lisp
CL-USER> (u:socket-receive socket (make-string 10) nil)
"world\0\0\0"
5
#(127 0 0 1)
8001
```

The final state of the terminal should be

```sh
$ nc -luv localhost 8001
Listening on [localhost] (family 0, port 8001)
Connection from localhost 60367 received!
helloworld
```

### More examples

See [the Cookbook](http://lispcookbook.github.io/cl-cookbook/sockets.html).

### Conventions

Specification of a `host` or `local-host` parameter
A `host` or `local-host` parameter may be any one of

-   32-bit positive integer,
-   A four element integer list representing IPv4 address, i.e.
    `#(127 0 0 1)`
-   a string containing an IP addres in dotted notation, or
-   a host name to be resolved through DNS lookup.

### Sockets and Servers

The system consists of two subsystems `usocket` and `usocket-server`. Loading `usocket`
avoids loading the server related functions and helps keeps the core small.
To load the whole thing, one needs to `:depends-on` both `"usocket"` and `"usocket-server"`
in the ASDF file. Both these work with the `:usocket` package.

### Sockets

**Types of Sockets**

- [usocket](#usocket)
- [datagram-usocket](#datagram-usocket)
- [stream-usocket](#stream-usocket)
- [stream-server-usocket](#stream-server-usocket)

**Socket Functions**

- [socket-connect](#socket-connect)
- [socket-listen](#socket-listen)
- [socket-accept](#socket-accept)
- [socket-close](#socket-close)
- [socket-send](#socket-send)
- [socket-receive](#socket-receive)
- [socket-shutdown](#socket-shutdown)

**Accessors**

- [get-local-name](#get-local-name)
- [get-local-address](#get-local-address)
- [get-local-port](#get-local-port)
- [get-peer-name](#get-peer-name)
- [get-peer-address](#get-peer-address)
- [get-peer-port](#get-peer-port)

**Select**

- [wait-for-input](#wait-for-input)

### Servers

The following are available in the `usocket` package after loading both `usocket` and `usocket-server` systems.

- [socket-server](#socket-server)
<br/>
<br/>
- [echo-tcp-handler](#echo-tcp-handler)
<br/>
<br/>
- [default-udp-handler](#default-udp-handler)
- [default-tcp-handler](#default-tcp-handler)

## API DOCUMENTATION

*`usocket` maintainers are committed to the interface described below for
the entire 0.x phase of the library. When 1.0 comes some of the
functionality may be split up in different functions and guarantees may
change because of it.*

### \*auto-port\*

```lisp
Variable
Default Value: 0
```

The port number to use with [socket-listen](#socket-listen) to make the socket listen
    on a random available port. The port number assigned can be
    retrieved from the returned socket by calling
    [get-local-port](#get-local-port).


### \*backend\*

```lisp
Variable
Default Value: :NATIVE
```

### \*default-event-base\*

No documentation found for `*default-event-base*`

### \*remote-host\*

Special variable used in [socket-server](#socket-server)'s
    handler function for getting current client address. (Start from
    USOCKET 0.5)

### \*remote-port\*

Special variable used in [socket-server](#socket-server)'s
    handler function for getting current client port. (Start from
    USOCKET 0.5)

### \*version\*

```lisp
Variable
Default Value: "0.8.3"
```

usocket version string

### \*wildcard-host\*

```lisp
Variable
Default Value: #(0 0 0 0)
```

Hostname to pass when all interfaces in the current system are to
  be bound.  If this variable is passed to [socket-listen](#socket-listen), IPv6 capable
  systems will also listen for IPv6 connections.

### +max-datagram-packet-size+

```lisp
Constant: 65507
```

The theoretical maximum amount of data in a UDP datagram.

The IPv4 UDP packets have a 16-bit length constraint, and IP+UDP header has 28-byte.

```
IP_MAXPACKET = 65535,       /* netinet/ip.h */
sizeof(struct ip) = 20,     /* netinet/ip.h */
sizeof(struct udphdr) = 8,  /* netinet/udp.h */

65535 - 20 - 8 = 65507
```

(But for UDP broadcast, the maximum message size is limited by the MTU size of the underlying link)

### add-waiter

```lisp
Function: (add-waiter wait-list input)
```

### address-in-use-error

```lisp
Condition
```


### address-not-available-error

```lisp
Condition
```


### bad-file-descriptor-error

```lisp
Condition
```


### connection-aborted-error

```lisp
Condition
```


### connection-refused-error

```lisp
Condition
```


### connection-reset-error

```lisp
Condition
```


### datagram-usocket

```lisp
Class
```

UDP (inet-datagram) socket

-  Direct superclasses: [usocket](#usocket)

<u>**Direct Slots**</u>

**connected-p**

```lisp
Initargs: :CONNECTED-P
Reader: CONNECTED-P
Writer: (SETF CONNECTED-P)
```
Used to identify if the datagram is connected. It will be setup
by [socket-connect](#socket-connect), and used by [socket-send](#socket-send)
and [socket-receive](#socket-receive).

### datagram-usocket-p

```lisp
Function: (datagram-usocket-p socket)
```

### deadline-timeout-error

```lisp
Condition
```


### default-tcp-handler

```lisp
Function: (default-tcp-handler stream)
```

### default-udp-handler

```lisp
Function: (default-udp-handler buffer)
```

### dotted-quad-to-vector-quad

```lisp
Function: (dotted-quad-to-vector-quad string)
```

### echo-tcp-handler

```lisp
Function: (echo-tcp-handler stream)
```

### get-host-by-name

```lisp
Function: (get-host-by-name name)
```

0.7.1+: if there're IPv4 addresses, return the first IPv4 address.

### get-hosts-by-name

```lisp
Function: (get-hosts-by-name name)
```

### get-local-address

```lisp
Generic Function: (get-local-address socket)
```

Returns the IP address of the socket.

### get-local-name

```lisp
Generic Function: (get-local-name socket)
```

Returns the IP address and port of the socket as values.

This function applies to both [stream-usocket](#stream-usocket) and server-stream-usocket
type objects.

### get-local-port

```lisp
Generic Function: (get-local-port socket)
```

Returns the IP port of the socket.

This function applies to both [stream-usocket](#stream-usocket) and server-stream-usocket
type objects.

### get-peer-address

```lisp
Generic Function: (get-peer-address socket)
```

Returns the IP address of the peer the `socket` is connected to.

### get-peer-name

```lisp
Generic Function: (get-peer-name socket)
```

Returns the IP address and port of the peer
the `socket` is connected to as values.

### get-peer-port

```lisp
Generic Function: (get-peer-port socket)
```

Returns the IP port of the peer the `socket` to.

### get-random-host-by-name

```lisp
Function: (get-random-host-by-name name)
```

0.7.1+: if there're IPv4 addresses, only return a random IPv4 address.

### hbo-to-dotted-quad

```lisp
Function: (hbo-to-dotted-quad integer)
```

Host-byte-order integer to dotted-quad string conversion utility.

### hbo-to-vector-quad

```lisp
Function: (hbo-to-vector-quad integer)
```

Host-byte-order integer to dotted-quad string conversion utility.

### host-byte-order

```lisp
Generic Function: (host-byte-order address)
```

### host-down-error

```lisp
Condition
```


### host-or-ip

```lisp
Generic Function: (host-or-ip condition)
```

### host-to-hostname

```lisp
Function: (host-to-hostname host)
```

Translate a string, vector quad or 16 byte IPv6 address to a
stringified hostname.

### host-unreachable-error

```lisp
Condition
```


### insufficient-implementation

```lisp
Condition
```

The ancestor of all errors usocket may generate
because of insufficient support from the underlying implementation
with respect to the arguments given to `function`.

One call may signal several errors, if the caller allows processing
to continue.



### integer-to-octet-buffer

```lisp
Function: (integer-to-octet-buffer integer buffer octets &key (start 0))
```

### interrupted-condition

```lisp
Condition
```


### invalid-argument-error

```lisp
Condition
```


### invalid-socket-error

```lisp
Condition
```


### invalid-socket-stream-error

```lisp
Condition
```


### ip-from-octet-buffer

```lisp
Macro: (ip-from-octet-buffer buffer &key (start 0))
```

### ip-to-octet-buffer

```lisp
Macro: (ip-to-octet-buffer ip buffer &key (start 0))
```

### ip/=

```lisp
Function: (ip/= ip1 ip2)
```

### ip=

```lisp
Function: (ip= ip1 ip2)
```

### ipv6-host-to-vector

```lisp
Function: (ipv6-host-to-vector string)
```

### make-wait-list

```lisp
Function: (make-wait-list waiters)
```

### network-down-error

```lisp
Condition
```


### network-reset-error

```lisp
Condition
```


### network-unreachable-error

```lisp
Condition
```


### no-buffers-error

```lisp
Condition
```


### ns-condition

```lisp
Condition
```

Parent condition for all name resolution conditions.

<u>**Direct Slots**</u>

**host-or-ip**
```lisp
Initargs: :HOST-OR-IP
Readers: HOST-OR-IP
Writers: (SETF HOST-OR-IP)
```

### ns-error

```lisp
Condition
```

Parent error for all name resolution errors.


### ns-host-not-found-error

```lisp
Condition
```


### ns-no-recovery-error

```lisp
Condition
```


### ns-try-again-condition

```lisp
Condition
```


### ns-unknown-condition

```lisp
Condition
```

Condition raised when there's no other - more applicable -
condition available.


### ns-unknown-error

```lisp
Condition
```

Error raised when there's no other - more applicable -
error available.


### octet-buffer-to-integer

```lisp
Function: (octet-buffer-to-integer buffer octets &key (start 0))
```

### operation-not-permitted-error

```lisp
Condition
```


### operation-not-supported-error

```lisp
Condition
```


### port-from-octet-buffer

```lisp
Macro: (port-from-octet-buffer buffer &key (start 0))
```

### port-to-octet-buffer

```lisp
Macro: (port-to-octet-buffer port buffer &key (start 0))
```

### protocol-not-supported-error

```lisp
Condition
```


### remove-all-waiters

```lisp
Function: (remove-all-waiters wait-list)
```

### remove-waiter

```lisp
Function: (remove-waiter wait-list input)
```

### shutdown-error

```lisp
Condition
```


### socket

```lisp
Generic Function: (socket object)
```

### socket-accept

```lisp
Generic Function: (socket-accept socket &key element-type)
```

Accepts a connection from `socket`, returning a [stream-socket](#stream-socket).

The stream associated with the socket returned has `element-type` when
explicitly specified, or the `element-type` of the `socket` (as used when it was created by the call to [socket-listen](#socket-listen)) otherwise.

### socket-close

```lisp
Generic Function: (socket-close usocket)
```

Close a previously opened [usocket](#usocket) after flushing it.

### socket-condition

```lisp
Condition
```

Parent condition for all socket related conditions.

<u>**Direct Slots**</u>

**socket**
```lisp
Initargs: :SOCKET
Readers: :USOCKET-SOCKET
Writers: (SETF :USOCKET-SOCKET)
```

### socket-connect

```lisp
Function: (socket-connect host port &key (protocol :stream)
           (element-type (quote character)) timeout deadline
           (nodelay t nodelay-specified) local-host local-port &aux
           (sockopt-tcp-nodelay-p (fboundp (quote sockopt-tcp-nodelay))))
```

Returns a [stream-usocket](#stream-usocket) (TCP) object if `protocol` is `:stream` and
[datagram-socket](#datagram-socket) (UDP) if it is `:datagram`.

Connects the socket to `host` on `port`.  `host` is assumed to be a string or
an IP address represented in vector notation, such as `#(192 168 1 1)`.
`port` is assumed to be an integer.

`element-type` specifies the element type to use when constructing the
stream associated with the stream-socket.  The default is `'character`,
but `(unsigned-byte 8)` is also a valid value.

`timeout` is a integer, it represents the socket option
    `SO_RCVTIMEO` (read timeout), in seconds.

`deadline` is only supported in Clozure CL and Digitool MCL

- `local-host` and `local-port`, when specified, will cause the socket
    calling bind() on local address. This is useful for selecting
    interfaces to send, or listening on UDP port. Note: use only one of
    them are allowed when reasonable (listen on wildcard address, or
    bind to random free port).

`nodelay` allows to disable/enable [Nagle's algorithm](http://en.wikipedia.org/wiki/Nagle%27s_algorithm).
If this parameter is omitted, the behaviour is inherited from the
CL implementation (in most cases, Nagle's algorithm is
enabled by default, but for example in ACL it is disabled).
If the parameter is specified, one of these three values is possible:

- `T` - Disable Nagle's algorithm; signals an [unsupported](#unsupported)
      condition if the implementation does not support explicit
      manipulation with that option.
- `NIL` - Leave Nagle's algorithm enabled on the socket;
      signals an [unsupported](#unsupported) condition if the implementation does
      not support explicit manipulation with that option.
- `:IF-SUPPORTED` - Disables Nagle's algorithm if the implementation
      allows this, otherwises just ignore this option.

### socket-error

```lisp
Condition
```

Parent error for all socket related errors


### socket-listen

```lisp
Function: (socket-listen host port &key reuseaddress
           (reuse-address NIL reuse-address-supplied-p) (backlog 5)
           (element-type (quote character)))
```

Bind to interface `host` on `port`. `host` should be the
representation of an ready-interface address.  The implementation is
not required to do an address lookup, making no guarantees that
hostnames will be correctly resolved.  If [\*wildcard-host\*](#wildcard-host) or `NIL` is
passed for host, the socket will be bound to all available
interfaces for the system.  port can be selected by the IP stack by
passing [\*auto-port\*](#auto-port). A `port` value of 0 indicates a request for
a random free port.

Returns an object of type [stream-server-usocket](#stream-server-usocket).

`reuse-address` and `backlog` are advisory parameters for setting socket
options at creation time. `element-type` is the element type of the
streams to be created by [socket-accept](#socket-accept).  `reuseaddress` is supported for
backward compatibility (but deprecated); when both `reuseaddress` and
`reuse-address` have been specified, the latter takes precedence.


### socket-option

```lisp
Generic Function: (socket-option socket option &key)
```

Get a socket's internal options

### socket-receive

```lisp
Generic Function: (socket-receive usocket buffer length &key element-type)
```

Receive packets from a previously opened [usocket](#usocket).

Returns 4 values: `(values buffer size host port)`

Receive data from a datagram socket, and return 4 values:
    `return-buffer`, `return-length`, `remote-host`, and `remove-port`.

If the datagram socket was created by [socket-connect](#socket-connect)
    with a `timeout` keyword argument, this function will block at most
    that timeout value (in seconds). (Start from USOCKET 0.5)

*socket* should be a [`datagram-usocket`](#datagram-usocket)

`buffer` is a Lisp vector of type `(simple-array (unsigned-byte 8) *)`. Using `nil` here
    is also allowed, new buffer will be created to hold data.

`length` is used to specify the length of a exist buffer for
    receiving at most these data. Using `nil` here is allowed, and the
    actual length of `buffer` will be used; when `buffer` is also `nil`,
    a default maximum length (65507) will be used.

### socket-send

```lisp
Generic Function: (socket-send usocket buffer length &key host port offset)
```

Send packets through a previously opened [usocket](#usocket).

`buffer` is a Lisp vector, type of
    `(simple-array (unsigned-byte 8) *)`.

`length` is used to tell [socket-send](#socket-send) the actual useful length of data buffer for sending to socket.

`host` and `port` are used for unconnected datagram sockets, for
    sending to specific destination.

The return value indicates the number of bytes sent. (Start from USOCKET 0.5)

### socket-server


```lisp
Function: (socket-server host port function &optional arguments &key
           in-new-thread (protocol :stream) (timeout 1)
           (max-buffer-size +max-datagram-packet-size+) element-type
           (reuse-address t) multi-threading name)
```
 Create a simple TCP or UDP socket server. (Start from USOCKET 0.5)

-   `host` names a local interface,
-   `port` names a local port,

-   `function` names a function object, which is used to handle TCP or
    UDP connections. This is explained below.

-   `arguments` is a list used for passing extra arguments to
    user-defined `function`.

-   `in-new-thread` is a boolean, default is `nil`. When it's `T`, the
    server will be created in a new thread and socket-server returns
    immediately in current thread.

-   `protocol` could be either `:stream` (default) or `:datagram`, which
    decide the socket server is TCP server or UDP server.

-   `timeout` is UDP only, it provides the internal
    [socket-receive](#socket-receive)
    call (in UDP event loop of the socket server) a read timeout,
    default value is 1 (second).

-   `max-buffer-size` is UDP only, it's the max UDP data buffer size
    when handling UDP packets, default value is 65507.
-   `element-type` is TCP only, it's element-type of the stream
    provided for user-defined function,
-   `reuse-address` is TCP only, it's a boolean option for internal
    call of socket-listen in the socket server,
-   `multi-threading` is TCP only, it's a boolean, default value is
    `nil`. When it's `T`, each client connection will cause a new
    thread being created to handle that client, so that the TCP server
    could handle multiple clients at the same time. (Note: since UDP
    server is connectionless, it can always handle multiple clients, as
    long as the handler function run fast enough)


**TCP HANDLER FUNCTION**

Following forms a valid template function for TCP:

        (defun default-tcp-handler (stream) ; null
          (declare (type stream stream))
          (terpri stream))

**Note:**

1. You don't need to close the stream as
    `socket-server` will do that for you.

2. More function arguments can be defined, and
    these extra arguments must be feeded as the optional `arguments` of
    `socket-server`

**UDP HANDLER FUNCTION**

The handler function for UDP is buffer-based, that is, you receive a
    buffer of data as input, and you return another buffer for output. A
    template function is a simple UDP echo server:

        (defun default-udp-handler (buffer) ; echo
          (declare (type (simple-array (unsigned-byte 8) *) buffer))
          buffer)

**Note:**

1. data length is the length of the whole buffer.

2. Sometimes you may want to know the client's IP address and sending port,
    these informations are specially bounded on variables
    [\*remote-host\*](#remote-host)
    and
    [\*remote-port\*](#remote-port)
    when handler function is running.


### socket-shutdown

```lisp
Generic Function: (socket-shutdown usocket direction)
```

The `socket-shutdown` call causes all or part of a full-duplex
    connection on the socket associated with sockfd to be shut down.

- If `direction` is `:input`, further receptions will be disallowed.
- If `direction` is `:output`, further transmissions will be disallowed.
- If `direction` is `:io`, further receptions and transmissions will be
    disallowed. (Starting from USOCKET 0.6.4)

`socket` should be a [datagram-usocket](#datagram-socket).

### socket-state

```lisp
Generic Function: (socket-state socket)
```
```text
NIL          - not ready
:READ        - ready to read
:READ-WRITE  - ready to read and write
:WRITE       - ready to write
```

### socket-stream

```lisp
Generic Function: (socket-stream object)
```

### socket-type-not-supported-error

```lisp
Condition
```


### socket-warning

No documentation found for `socket-warning`


### stream-server-usocket

```lisp
Class
```

Socket which listens for stream connections to
be initiated from remote sockets.

-  Direct superclasses: [usocket](#usocket)

<u>**Direct Slots**</u>

**element-type**

```lisp
Initargs: :ELEMENT-TYPE
Initform: 'CHARACTER
Reader: ELEMENT-TYPE
```

Indicates the default element-type to be used when constructing
streams off this socket when no element type is specified in the
call to [socket-accept](#socket-accept).

### stream-server-usocket-p

```lisp
Function: (stream-server-usocket-p socket)
```

### stream-usocket

```lisp
Class
```

Stream socket class. Contrary to other sockets, these sockets may be closed either
with the `socket-close` method or by closing the associated stream
(which can be retrieved with the `socket-stream` accessor).

-  Direct superclasses: [usocket](#usocket)

<u>**Direct Slots**</u>

**stream**
```lisp
Initargs: :STREAM
Readers: SOCKET-STREAM
Writers: (SETF SOCKET-STREAM)
```

Used to store the stream associated with the tcp socket
connection. When you want to write to the socket stream, use this function.

### stream-usocket-p

```lisp
Function: (stream-usocket-p socket)
```

### timeout-error

```lisp
Condition
```


### unimplemented

```lisp
Macro: (unimplemented feature context)
```

```lisp
Condition
```

Signalled if a certain feature might be implemented,
based on the features of the underlying implementation, but hasn't
been implemented yet.


### unknown-condition

```lisp
Condition
```

Condition raised when there's no other - more applicable -
condition available.


### unknown-error

```lisp
Condition
```

Error raised when there's no other - more applicable -
error available.


### unsupported

```lisp
Macro: (unsupported feature context &key minimum)
```

```lisp
Condition
```

Signalled when the underlying implementation
doesn't allow supporting the requested feature.

When you see this error, go bug your vendor/implementation developer!


### usocket

```lisp
Class
```

The main socket class.

Sockets should be closed using the [socket-close](#socket-close) method.

<u>**Direct Slots**</u>

**socket**
```lisp
Initargs: :SOCKET
Readers: SOCKET
Writers: (SETF SOCKET)
```

Used to store sockets as used by the current implementation -
may be any of socket handles, socket objects and stream objects

**state**
```lisp
Initargs: :STATE
Readers: STATE
Writers: (SETF STATE)
```

Used to store socket state: `NIL` (not ready), or `:READ` (ready to read).

### usocket-p

```lisp
Function: (usocket-p socket)
```

### vector-quad-to-dotted-quad

```lisp
Function: (vector-quad-to-dotted-quad vector)
```

### vector-to-ipv6-host

```lisp
Function: (vector-to-ipv6-host vector)
```

### wait-for-input

```lisp
Function: (wait-for-input socket-or-sockets &key timeout ready-only &aux
           (single-socket-p (usocket-p socket-or-sockets)))
```

Waits for one or more sockets to become ready for reading from
the socket. This is like the UNIX's "select" function.

When `timeout` (a non-negative real number) is
specified, wait `timeout` seconds, or wait indefinitely when
it isn't specified.  A timeout value of 0 (zero) means polling.

Returns two values:

- the first value is the list of sockets which
are readable (or in case of server sockets acceptable).  `NIL` may
be returned for this value either when waiting timed out or when
it was interrupted (EINTR).
- The second value is a real number
indicating the time remaining within the timeout period or `NIL` if
none.

Without the `ready-only` arg, `wait-for-input` will return all sockets in
the original list you passed it. This prevents a new list from being
consed up. Some users of USOCKET were reluctant to use it if it
wouldn't behave that way, expecting it to cost significant performance
to do the associated garbage collection.

Without the `ready-only` arg, you need to check the socket `state` slot for
the values documented in usocket.lisp in the usocket class.

### with-client-socket

```lisp
Macro: (with-client-socket (socket-var stream-var &rest socket-connect-args)
        &body body)
```

Bind the socket resulting from a call to [socket-connect](#socket-connect) with
the arguments socket-connect-args to socket-var and if stream-var is
non-nil, bind the associated socket stream to it.

### with-connected-socket

```lisp
Macro: (with-connected-socket (var socket) &body body)
```

Bind [socket](#socket) to var, ensuring socket destruction on exit.

body is only evaluated when var is bound to a non-null value.

The body is an implied progn form.

### with-mapped-conditions

```lisp
Macro: (with-mapped-conditions (&optional socket host-or-ip) &body body)
```

### with-server-socket

```lisp
Macro: (with-server-socket (var server-socket) &body body)
```

Bind server-socket to var, ensuring socket destruction on exit.

body is only evaluated when var is bound to a non-null value.

The body is an implied progn form.

### with-socket-listener

```lisp
Macro: (with-socket-listener (socket-var &rest socket-listen-args) &body body)
```

Bind the socket resulting from a call to [socket-listen](#socket-listen) with arguments
socket-listen-args to socket-var.
