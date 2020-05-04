# hunchentoot - Web Server

Version: 1.2.38
<br/>
Repository: [edicl/hunchentoot - Github](https://github.com/edicl/hunchentoot/)

*This page was possible due to the excellent [official documentation](https://edicl.github.io/hunchentoot/) as well as the page on [Web Development on The Common Lisp Cookbook](http://lispcookbook.github.io/cl-cookbook/web.html).*

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/digikar99/common-lisp.readthedocs/issues).*

***

*hunchentoot was formerly known as TBNL.

## INTRODUCTION

Hunchentoot is a web server written in Common Lisp and a toolkit for building
dynamic websites.  As a stand-alone web
server, Hunchentoot is capable of HTTP/1.1 chunking (both directions),
persistent connections (keep-alive), and SSL.

Hunchentoot provides facilities like automatic session handling (with
and without cookies), logging, customizable error handling, and easy
access to GET and POST parameters sent by the client. It does *not*
include functionality to programmatically generate HTML output. For
this task you can use any library you like,
e.g. [CL-WHO](https://github.com/edicl/cl-who/) or
[HTML-TEMPLATE](https://github.com/edicl/html-template/).

Hunchentoot should work with most popular lisp implementations including 
SBCL, CCL, LispWorks and all
Lisps which are supported by the compatibility layers
[usocket](http://common-lisp.net/project/usocket/) and [Bordeaux
Threads](http://common-lisp.net/project/bordeaux-threads/). 

Hunchentoot talks with its front-end or with the client over TCP/IP
sockets and optionally uses multiprocessing to handle several requests
at the same time.  Therefore, it cannot be implemented completely in
[portable Common
Lisp](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm).


Hunchentoot comes with a [BSD-style
license](http://www.opensource.org/licenses/bsd-license.php) so you
can basically do with it whatever you want.

Official documentation for Hunchentoot can be found in the `docs` directory or at the [project
website](https://edicl.github.io/hunchentoot/).


## GETTING STARTED

### Installation using quicklisp

See [the section on Installation under Defacto Libraries on Home Page](../index.html#defacto-installation).

### Serving local files

To start the server, simply

```lisp
(defvar *acceptor*)
(setq *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start *acceptor*)
```

You should see something - but not very interesting - at
"[`http://127.0.0.1:4242/`](http://127.0.0.1:4242/)" in your browser.

By default, Hunchentoot serves files from the `www/` directory from
its source tree.  In the distribution, that directory contains a HTML
version of the documentation as well as the error templates. If installed via
quicklisp, see `(ql:where-is-system "hunchentoot")`.

- See [acceptor slots](#acceptor-slots) for a list of various `slots` (with or
without `initargs`). 

- And [configuration variables](#configuration-variables)
for a list of various configuration options for hunchentoot, such as whether to
`*catch-errors-p` or `*log-lisp-errors-p*` and more.

- Acceptors provided with hunchentoot:
  - [acceptor](#acceptor)
  - [easy-acceptor](#easy-acceptor)
  - [ssl-acceptor](#ssl-acceptor)
  - [easy-ssl-acceptor](#easy-ssl-acceptor)

### Going dynamic

#### create-X-dispatcher

To bind an existing function to a route, we [create-prefix-dispatcher](#create-prefix-dispatcher)
that we push onto the [\*dispatch-table\*](#dispatch-table) 
(just a global list of dispatch functions):

```lisp
(defun hello ()
  (format nil "Hello, it works!"))

(push
  (hunchentoot:create-prefix-dispatcher "/hello.html" 'hello) 
  hunchentoot:*dispatch-table*)
```

To create a route with a regexp, we use [create-regex-dispatcher](#create-regex-dispatcher), where the url-as-regexp can be a string, an s-expression or a cl-ppcre scanner. In all, there exist

- [create-folder-dispatcher-and-handler](#create-folder-dispatcher-and-handler)
- [create-prefix-dispatcher](#create-prefix-dispatcher)
- [create-regex-dispatcher](#create-regex-dispatcher)
- [create-static-file-dispatcher-and-handler](#create-static-file-dispatcher-and-handler)

#### define-easy-handler

`define-easy-handler` allows to create a function and to bind it to an uri at once. For instance:

~~~lisp
(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))
~~~

Visit [http://localhost:4242/yo](http://localhost:4242/yo) or add parameters to the url:
[http://localhost:4242/yo?name=Alice](http://localhost:4242/yo?name=Alice).

Note that we didn't explicitly ask Hunchentoot to add this
route to our first acceptor of the port 4242. This handler also works for another acceptor, 
say another one opened at port 4444: 
[http://localhost:4444/yo?name=Bob](http://localhost:4444/yo?name=Bob) In fact, 
`define-easy-handler` accepts an `acceptor-names` parameter that defines which acceptors it works for.

#### Accessing GET and POST parameters

Query parameters are accessible with

~~~lisp
(hunchentoot:parameter "my-param")
~~~

while in the [context of a request](#variables-in-request-context).

It acts on the default `*request*` object which is passed to all handlers.

There also are [get-paramater](#get-parameter) and [post-parameter](#post-parameter). 

See also the [Variables in the context of a request](#variables-in-request-context).

### More documentation, tutorials and add-ons

- Adam Petersen has written a book called ["Lisp for the
Web"](http://www.adampetersen.se/articles/lispweb.htm) which explains
how Hunchentoot and some other libraries can be used to build web sites.
- See also the [Web Development - Cookbook](http://lispcookbook.github.io/cl-cookbook/web.html).
- [Implementing a blog in Common Lisp - Vetle Roeim](https://roeim.net/vetle/docs/cl-webapp-intro/part-1/)

Extensions and related softwares:

-   [Clack](https://github.com/fukamachi/clack) is a web server
    abstraction layer, defaulting to Hunchentoot.
-   [hunchentoot-cgi](https://github.com/slyrus/hunchentoot-cgi) (by 
        Cyrus Harmo) provides
    [CGI](http://en.wikipedia.org/wiki/Common_Gateway_Interface)
    handlers for Hunchentoot.
-   [CL-WEBDAV](http://weitz.de/cl-webdav/) is a
    [WebDAV](http://webdav.org/) server based on Hunchentoot.
-   [RESTAS](http://restas.lisper.ru/) is a web framework based on
    Hunchentoot. [Caveman](https://github.com/fukamachi/caveman),
    [Radiance](https://github.com/Shirakumo/radiance),
    [Snooze](https://github.com/joaotavora/snooze) or again
    [Weblocks](http://40ants.com/weblocks/) are frameworks compatible
    with it.


## API REFERENCE

### 1. <span id="acceptor">ACCEPTOR</span>

```lisp
Class
```

To create a Hunchentoot webserver, you make an
instance of this class and use the generic function START to start it
(and STOP to stop it).  Use the :PORT initarg if you don't want to
listen on the default http port 80.  There are other initargs most of
which you probably won't need very often.  They are explained in
detail in the docstrings of the slot definitions for this class.

Unless you are in a Lisp without MP capabilities, you can have several
active instances of ACCEPTOR (listening on different ports) at the
same time.

- Direct superclasses: STANDARD-OBJECT
- Direct subclasses: [EASY-ACCEPTOR](#easy-acceptor), [SSL-ACCEPTOR](#ssl-acceptor)

#### RELEVANT METHODS

##### accept-connections

```lisp
Function: (accept-connections acceptor)
```
In a loop, accepts a connection and hands it over
to the acceptor's taskmaster for processing using
HANDLE-INCOMING-CONNECTION.  On LispWorks, this function returns
immediately, on other Lisps it retusn only once the acceptor has been
stopped.

##### acceptor-log-access

```lisp
Function: (acceptor-log-access acceptor &key return-code)
```
Function to call to log access to the acceptor.  The RETURN-CODE,
CONTENT and CONTENT-LENGTH keyword arguments contain additional
information about the request to log.  In addition, it can use the
standard request accessor functions that are available to handler
functions to find out more information about the request.

##### acceptor-log-message

```lisp
Function: (acceptor-log-message acceptor log-level format-string &rest format-arguments)
```

Function to call to log messages by the ACCEPTOR.  It must accept
a severity level for the message, which will be one of :ERROR, :INFO,
or :WARNING, a format string and an arbitary number of formatting
arguments.

##### acceptor-status-message

```lisp
Function: (acceptor-status-message acceptor http-status-code &key &allow-other-keys)
```

This function is called after the request's handler has been
invoked to convert the HTTP-STATUS-CODE to a HTML message to be
displayed to the user.  If this function returns a string, that
string is sent to the client instead of the content produced by the
handler, if any.

If an ERROR-TEMPLATE-DIRECTORY is set in the current acceptor and
the directory contains a file corresponding to HTTP-STATUS-CODE
named \<code\>.html, that file is sent to the client after variable
substitution.  Variables are referenced by ${<variable-name>}.

Additional keyword arguments may be provided which are made
available to the templating logic as substitution variables.  These
variables can be interpolated into error message templates in,
which contains the current URL relative to the server and without
GET parameters.

In addition to the variables corresponding to keyword arguments,
the script-name, lisp-implementation-type,
lisp-implementation-version and hunchentoot-version variables are
available.

##### detach-socket

```lisp
Function: (detach-socket acceptor)
```

Indicate to Hunchentoot that it should stop serving
requests on the current request's socket.
Hunchentoot will finish processing the current
request and then return from PROCESS-CONNECTION
without closing the connection to the client.
DETACH-SOCKET can only be called from within a
request handler function.

##### initialize-connection-stream

```lisp
Function: (initialize-connection-stream acceptor stream)
```

Can be used to modify the stream which is used to
communicate between client and server before the request is read.  The
default method of ACCEPTOR does nothing, but see for example the
method defined for SSL-ACCEPTOR.  All methods of this generic function
must return the stream to use.

##### process-connection

```lisp
Function: (process-connection acceptor socket)
```

This function is called by the taskmaster when a
new client connection has been established.  Its arguments are the
ACCEPTOR object and a LispWorks socket handle or a usocket socket
stream object in SOCKET.  It reads the request headers, sets up the
request and reply objects, and hands over to PROCESS-REQUEST.  This is
done in a loop until the stream has to be closed or until a connection
timeout occurs.

It is probably not a good idea to re-implement this method until you
really, really know what you're doing.

Handlers may call to the `DETACH-SOCKET`generic function to
indicate that no further requests should be handled on the connection
by Hunchentoot, and that responsibility for the socket is assumed by
third-party software. This can be used by specialized handlers that
wish to hand over connection polling or processing to functions
outside of Hunchentoot, i.e. for connection multiplexing or
implementing specialized client protocols. Hunchentoot will finish
processing the request and the `PROCESS-CONNECTION` function
will return without closing the connection. At that point, the
acceptor may interact with the socket in whatever fashion required.

##### reset-connection-stream

```lisp
Function: (reset-connection-stream acceptor stream)
```

Resets the stream which is used to communicate
between client and server after one request has been served so that it
can be used to process the next request.  This generic function is
called after a request has been processed and must return the
stream.

##### start

```lisp
Function: (start acceptor)
```

Starts the ACCEPTOR so that it begins accepting
connections.  Returns the acceptor.

##### start-listening

```lisp
Function: (start-listening acceptor)
```

Sets up a listen socket for the given ACCEPTOR and
enables it to listen to incoming connections.  This function is called
from the thread that starts the acceptor initially and may return
errors resulting from the listening operation (like 'address in use'
or similar).

##### stop

```lisp
Function: (stop acceptor &key soft)
```

Stops the ACCEPTOR so that it no longer accepts
requests.  If SOFT is true, and there are any requests in progress,
wait until all requests are fully processed, but meanwhile do not
accept new requests.  Note that SOFT must not be set when calling
STOP from within a request handler, as that will deadlock.

##### started-p

```lisp
Function: (started-p acceptor)
```

Tells if ACCEPTOR has been started.
The default implementation simply queries ACCEPTOR for its listening
status, so if T is returned to the calling thread, then some thread
has called START or some thread's call to STOP hasn't finished. If NIL
is returned either some thread has called STOP, or some thread's call
to START hasn't finished or START was never called at all for
ACCEPTOR.

#### <span id="acceptor-slots">SLOTS</span>

##### acceptor-shutdown-p

```lisp
Initform: T
```

 A flag that makes the acceptor
 shutdown itself when set to something other than NIL.

##### access-log-destination

```lisp
Initargs: :access-log-destination
Readers: hunchentoot:acceptor-access-log-destination
Writers: (setf hunchentoot:acceptor-access-log-destination)
```

 Destination of the access log
 which contains one log entry per request handled in a format similar
 to Apache's access.log.  Can be set to a pathname or string
 designating the log file, to a open output stream or to NIL to
 suppress logging.

##### address

```lisp
Initargs: :address
Readers: hunchentoot:acceptor-address
```

 The address the acceptor is listening on.
 If address is a string denoting an IP address, then the server only
 receives connections for that address.  This must be one of the
 addresses associated with the machine and allowed values are host
 names such as "www.zappa.com" and address strings such as
 "72.3.247.29".  If address is NIL, then the server will receive
 connections to all IP addresses on the machine. This is the default.
 
##### document-root

```lisp
Initargs: :document-root
Readers: hunchentoot:acceptor-document-root
Writers: (setf hunchentoot:acceptor-document-root)
```

 Directory pathname that points to
 files that are served by the acceptor if no more specific
 acceptor-dispatch-request method handles the request.
 
##### error-template-directory

```lisp
Initargs: :error-template-directory
Readers: hunchentoot:acceptor-error-template-directory
Writers: (setf hunchentoot:acceptor-error-template-directory)
```

 Directory pathname that
  contains error message template files for server-generated error
  messages.  Files must be named <return-code>.html with <return-code>
  representing the HTTP return code that the file applies to,
  i.e. 404.html would be used as the content for a HTTP 404 Not found
  response.

##### listen-backlog

```lisp
Initargs: :listen-backlog
Readers: hunchentoot:acceptor-listen-backlog
```

 Number of pending connections
allowed in the listen socket before the kernel rejects
further incoming connections.

##### listen-socket

 The socket listening for incoming
 connections.



##### message-log-destination

```lisp
Initargs: :message-log-destination
Readers: hunchentoot:acceptor-message-log-destination
Writers: (setf hunchentoot:acceptor-message-log-destination)
```

 Destination of the server
 error log which is used to log informational, warning and error
 messages in a free-text format intended for human inspection. Can be
 set to a pathname or string designating the log file, to a open output
 stream or to NIL to suppress logging.


##### input-chunking-p

```lisp
Initargs: :input-chunking-p
Readers: hunchentoot:acceptor-input-chunking-p
Writers: (setf hunchentoot:acceptor-input-chunking-p)
```

 A generalized boolean denoting
 whether the acceptor may use chunked encoding for input, i.e. when
 accepting request bodies from the client.  The default is T and
 there's usually no reason to change this to NIL.

##### name

```lisp
Initargs: :name
Readers: hunchentoot:acceptor-name
Writers: (setf hunchentoot:acceptor-name)
```

 The optional name of the acceptor, a symbol.
 This name can be utilized when defining "easy handlers" - see
 DEFINE-EASY-HANDLER.  The default name is an uninterned symbol as
 returned by GENSYM.

##### output-chunking-p

```lisp
Initargs: :output-chunking-p
Readers: hunchentoot:acceptor-output-chunking-p
Writers: (setf hunchentoot:acceptor-output-chunking-p)
```

 A generalized boolean denoting
 whether the acceptor may use chunked encoding for output, i.e. when
 sending data to the client.  The default is T and there's usually no
 reason to change this to NIL.

##### persistent-connections-p

```lisp
Initargs: :persistent-connections-p
Readers: hunchentoot:acceptor-persistent-connections-p
Writers: (setf hunchentoot:acceptor-persistent-connections-p)
```

 A generalized boolean
 denoting whether the acceptor supports persistent connections, which
 is the default for threaded acceptors.  If this property is NIL,
 Hunchentoot closes each incoming connection after having processed one
 request.  This is the default for non-threaded acceptors.

##### port

```lisp
Initargs: :port
Readers: hunchentoot:acceptor-port
```

 The port the acceptor is listening on.  The
 default is 80.  Note that depending on your operating system you might
 need special privileges to listen on port 80.  When 0, the port will be
 chosen by the system the first time the acceptor is started.

##### read-timeout

```lisp
Initargs: :read-timeout
Readers: hunchentoot:acceptor-read-timeout
```

 The read timeout of the acceptor,
 specified in (fractional) seconds.  The precise semantics of this
 parameter is determined by the underlying Lisp's implementation of
 socket timeouts.  NIL means no timeout.

##### reply-class

```lisp
Initargs: :reply-class
Readers: hunchentoot:acceptor-reply-class
Writers: (setf hunchentoot:acceptor-reply-class)
```

 Determines which class of reply
 objects is created when a request is served in and should be (a
 symbol naming) a class which inherits from REPLY.  The default is the
 symbol REPLY.

##### request-class

```lisp
Initargs: :request-class
Readers: hunchentoot:acceptor-request-class
Writers: (setf hunchentoot:acceptor-request-class)
```

 Determines which class of request
 objects is created when a request comes in and should be (a symbol
 naming) a class which inherits from REQUEST.  The default is the
 symbol REQUEST.

##### requests-in-progress

```lisp
Initform: 0
```

 The number of
 requests currently in progress.

##### shutdown-lock

 The lock protecting the shutdown-queue
 condition variable and the requests-in-progress counter.

##### shutdown-queue

 A condition variable
 used with soft shutdown, signaled when all requests
 have been processed.

##### <span id="acceptor-taskmaster">taskmaster</span>

```lisp
Initargs: :taskmaster
```

 The taskmaster (i.e. an instance of a
 subclass of TASKMASTER) that is responsible for scheduling the work
 for this acceptor.  The default depends on the MP capabilities of the
 underlying Lisp.


##### write-timeout

```lisp
Initargs: :write-timeout
Readers: hunchentoot:acceptor-write-timeout
```

 The write timeout of the acceptor,
 specified in (fractional) seconds.  The precise semantics of this
 parameter is determined by the underlying Lisp's implementation of
 socket timeouts.  NIL means no timeout.



### 2. <span id="easy-acceptor">EASY-ACCEPTOR</span>


```lisp
Class
```

This is the acceptor of the "easy" Hunchentoot framework.

- Direct superclasses: [ACCEPTOR](#acceptor)
- Direct subclasses: [EASY-SSL-ACCEPTOR](#easy-ssl-acceptor)


### 3. <span id="define-easy-handler">DEFINE-EASY-HANDLER</span>

```lisp
Macro: (define-easy-handler description lambda-list &body body)
```
Defines a handler with the body BODY and optionally registers
it with a URI so that it will be found by [DISPATCH-EASY-HANDLERS](#DISPATCH-EASY-HANDLERS).
DESCRIPTION is either a symbol NAME or a list matching the
destructuring lambda list

```lisp
  (name &key uri acceptor-names default-parameter-type default-request-type)
```

LAMBDA-LIST is a list the elements of which are either a symbol
VAR or a list matching the destructuring lambda list

```lisp
  (var &key real-name parameter-type init-form request-type)
```

The resulting handler will be a Lisp function with the name NAME
and keyword parameters named by the VAR symbols.  Each VAR will
be bound to the value of the GET or POST parameter called
REAL-NAME (a string) before BODY is executed.  If REAL-NAME is
not provided, it will be computed by downcasing the symbol name
of VAR.

If URI (which is evaluated) is provided, then it must be a string or
a function designator for a function of one argument.  In this case,
the handler will be returned by DISPATCH-EASY-HANDLERS, if URI is a
string and the script name of a request is URI, or if URI designates a
function and applying this function to the current request object
returns a true value.

ACCEPTOR-NAMES (which is evaluated) can be a list of symbols which
means that the handler will be returned by DISPATCH-EASY-HANDLERS in
acceptors which have one of these names (see ACCEPTOR-NAME).
ACCEPTOR-NAMES can also be the symbol T which means that the handler
will be returned by DISPATCH-EASY-HANDLERS in every acceptor.

Whether the GET or POST parameter (or both) will be taken into
consideration, depends on REQUEST-TYPE which can
be :GET, :POST, :BOTH, or NIL.  In the last case, the value of
DEFAULT-REQUEST-TYPE (the default of which is :BOTH) will be
used.

The value of VAR will usually be a string (unless it resulted from a
file upload in which case it won't be converted at all), but if
PARAMETER-TYPE (which is evaluated) is provided, the string will be
converted to another Lisp type by the following rules:

If the corresponding GET or POST parameter wasn't provided by the
client, VAR's value will be NIL.  If PARAMETER-TYPE is 'STRING, VAR's
value remains as is.  If PARAMETER-TYPE is 'INTEGER and the parameter
string consists solely of decimal digits, VAR's value will be the
corresponding integer, otherwise NIL.  If PARAMETER-TYPE is 'KEYWORD,
VAR's value will be the keyword obtained by interning the upcased
parameter string into the keyword package.  If PARAMETER-TYPE is
'CHARACTER and the parameter string is of length one, VAR's value will
be the single character of this string, otherwise NIL.  If
PARAMETER-TYPE is 'BOOLEAN, VAR's value will always be T (unless it
is NIL by the first rule above, of course).  If PARAMETER-TYPE is any
other atom, it is supposed to be a function designator for a unary
function which will be called to convert the string to something else.

Those were the rules for `simple' types, but PARAMETER-TYPE can
also be a list starting with one of the symbols LIST, ARRAY, or
HASH-TABLE.  The second value of the list must always be a simple
parameter type as in the last paragraph - we'll call it the
`inner type' below.

In the case of 'LIST, all GET/POST parameters called REAL-NAME
will be collected, converted to the inner type, and assembled
into a list which will be the value of VAR.

In the case of 'ARRAY, all GET/POST parameters which have a name
like the result of

```lisp
  (format nil "~A[~A]" real-name n)
```

where N is a non-negative integer, will be assembled into an
array where the Nth element will be set accordingly, after
conversion to the inner type.  The array, which will become the
value of VAR, will be big enough to hold all matching parameters,
but not bigger.  Array elements not set as described above will
be NIL.  Note that VAR will always be bound to an array, which
may be empty, so it will never be NIL, even if no appropriate
GET/POST parameters are found.

The full form of a 'HASH-TABLE parameter type is

```lisp
  (hash-table inner-type key-type test-function),
```

but KEY-TYPE and TEST-FUNCTION can be left out in which case they
default to 'STRING and 'EQUAL, respectively.  For this parameter
type, all GET/POST parameters which have a name like the result
of

```lisp
  (format nil "~A{~A}" real-name key)
```

(where KEY is a string that doesn't contain curly brackets) will
become the values (after conversion to INNER-TYPE) of a hash
table with test function TEST-FUNCTION where KEY (after
conversion to KEY-TYPE) will be the corresponding key.  Note that
VAR will always be bound to a hash table, which may be empty, so
it will never be NIL, even if no appropriate GET/POST parameters
are found.

To make matters even more complicated, the three compound
parameter types also have an abbreviated form - just one of the
symbols LIST, ARRAY, or HASH-TABLE.  In this case, the inner type
will default to 'STRING.

If PARAMETER-TYPE is not provided or NIL, DEFAULT-PARAMETER-TYPE
(the default of which is 'STRING) will be used instead.

If the result of the computations above would be that VAR would
be bound to NIL, then INIT-FORM (if provided) will be evaluated
instead, and VAR will be bound to the result of this evaluation.

Handlers built with this macro are constructed in such a way that
the resulting Lisp function is useful even outside of
Hunchentoot.  Specifically, all the parameter computations above
will only happen if \*REQUEST* is bound, i.e. if we're within a
Hunchentoot request.  Otherwise, VAR will always be bound to the
result of evaluating INIT-FORM unless a corresponding keyword
argument is provided.


### 4. <span id="ssl-acceptor">SSL-ACCEPTOR</span>


```lisp
Class
```

Create and START an instance of this class
(instead of ACCEPTOR) if you want an https server.  There are two
required initargs, :SSL-CERTIFICATE-FILE and :SSL-PRIVATEKEY-FILE, for
pathname designators denoting the certificate file and the key file in
PEM format.  On LispWorks, you can have both in one file in which case
the second initarg is optional.  You can also use the
:SSL-PRIVATEKEY-PASSWORD initarg to provide a password (as a string)
for the key file (or NIL, the default, for no password).

The default port for SSL-ACCEPTOR instances is 443 instead of 80

-  Direct superclasses: [ACCEPTOR](#acceptor)
-  Direct subclasses: [EASY-SSL-ACCEPTOR](#easy-ssl-acceptor)

#### DIRECT SLOTS

Also see slots for the superclass.

##### ssl-certificate-file

```lisp
Initargs: :ssl-certificate-file
Readers: hunchentoot:acceptor-ssl-certificate-file
```

 A pathname designator for a
 certificate file in PEM format.

##### ssl-privatekey-file

```lisp
Initargs: :ssl-privatekey-file
Readers: hunchentoot:acceptor-ssl-privatekey-file
```

 A pathname designator for a
 private key file in PEM format, or (only on LispWorks) NIL if the
 certificate file contains the private key.

##### ssl-privatekey-password

```lisp
Initargs: :ssl-privatekey-password
Readers: hunchentoot:acceptor-ssl-privatekey-password
```

 The password for the
 private key file or NIL for no password.

### 5. <span id="easy-ssl-acceptor">EASY-SSL-ACCEPTOR</span>


```lisp
Class
```

This is an acceptor that mixes the "easy" Hunchentoot with SSL connections.

-  Direct superclasses: [EASY-ACCEPTOR](#easy-acceptor), [SSL-ACCEPTOR](#ssl-acceptor)
-  No subclasses.


### 6. <span id="class-reply">REPLY</span>


```lisp
Class
```

Objects of this class hold all the information
about an outgoing reply.  They are created automatically by
Hunchentoot and can be accessed and modified by the corresponding
handler.

You should not mess with the slots of these objects directly, but you
can subclass REPLY in order to implement your own behaviour.  See the
REPLY-CLASS slot of the ACCEPTOR class.

-  Direct superclasses: STANDARD-OBJECT
-  No subclasses.

#### RELEVANT METHODS

##### headers-out\*

```lisp
Function: (headers-out* &optional (reply *reply*))
```
Returns an alist of the outgoing headers associated with the
REPLY object REPLY.

##### content-length\*

```lisp
Function: (content-length* &optional (reply *reply*))
```
The outgoing 'Content-Length' http header of REPLY.

##### content-type\*

```lisp
Function: (content-type* &optional (reply *reply*))
```
The outgoing 'Content-Type' http header of REPLY.

##### cookie-out

```lisp
Function: (cookie-out name &optional (reply *reply*))
```
Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive.


##### cookies-out\*

```lisp
Function: (cookies-out* &optional (reply *reply*))
```
Returns an alist of the outgoing cookies associated with the
REPLY object REPLY.

##### return-code\*

```lisp
Function: (return-code* &optional (reply *reply*))
```
The http return code of REPLY.  The return codes Hunchentoot can
handle are defined in specials.lisp.

##### send-headers

```lisp
Function: (send-headers)
```
Sends the initial status line and all headers as determined by the
REPLY object \*REPLY\*.  Returns a binary stream to which the body of
the reply can be written.  Once this function has been called, further
changes to \*REPLY\* don't have any effect.  Also, automatic handling of
errors (i.e. sending the corresponding status code to the browser,
etc.) is turned off for this request.  If your handlers return the
full body as a string or as an array of octets you should NOT call
this function.

This function does not return control to the caller during HEAD
request processing.

 
##### reply-external-format\*

```lisp
Function: (reply-external-format* &optional (reply *reply*))
```
The external format of REPLY which is used for character output.


#### SLOTS

#####  content-type

```lisp
Readers: hunchentoot:content-type
```

 The outgoing 'Content-Type' http
 header which defaults to the value of \*DEFAULT-CONTENT-TYPE\*.

##### content-length

```lisp
Readers: hunchentoot:content-length
```

 The outgoing 'Content-Length'
 http header which defaults NIL.  If this is NIL, Hunchentoot will
 compute the content length.

##### headers-out

```lisp
Readers: hunchentoot:headers-out
```

 An alist of the outgoing http headers
 not including the 'Set-Cookie', 'Content-Length', and 'Content-Type'
 headers.  Use the functions HEADER-OUT and (SETF HEADER-OUT) to
 modify this slot.
 
##### return-code

```lisp
Initform: hunchentoot:+http-ok+
Readers: hunchentoot:return-code
Writers: (setf hunchentoot:return-code)
```

 The http return code of this
 reply.  The return codes Hunchentoot can handle are defined in
 specials.lisp.


##### external-format

```lisp
Initform: hunchentoot:*hunchentoot-default-external-format*
Readers: hunchentoot:reply-external-format
Writers: (setf hunchentoot:reply-external-format)
```

 The external format of the reply -
 used for character output.

##### cookies-out

```lisp
Readers: hunchentoot:cookies-out
Writers: (setf hunchentoot:cookies-out)
```

The outgoing cookies.  This slot's
 value should only be modified by the functions defined in
 cookies.lisp.

### 7. <span id="class-request">REQUEST</span>

```lisp
Class
```

Objects of this class hold all the information
about an incoming request.  They are created automatically by
acceptors and can be accessed by the corresponding handler.

You should not mess with the slots of these objects directly, but you
can subclass REQUEST in order to implement your own behaviour.  See
the REQUEST-CLASS slot of the ACCEPTOR class.

-  Direct superclasses: STANDARD-OBJECT
-  No subclasses.

#### RELEVANT METHODS

##### real-remote-addr

```lisp
Function: (real-remote-addr &optional (request *request*))
```
Returns the 'X-Forwarded-For' incoming http header as the
second value in the form of a list of IP addresses and the first
element of this list as the first value if this header exists.
Otherwise returns the value of REMOTE-ADDR as the only value.

##### parameter

```lisp
Function: (parameter name &optional (request *request*))
```
Returns the GET or the POST parameter with name NAME (a string) -
or NIL if there is none.  If both a GET and a POST parameter with the
same name exist the GET parameter is returned.  Search is
case-sensitive.

##### get-parameter

```lisp
Function: (get-parameter name &optional (request *request*))
```
Returns the GET parameter with name NAME (a string) - or NIL if
there is none.  Search is case-sensitive.

##### get-parameters\*

```lisp
Function: (get-parameters* &optional (request *request*))
```
Returns an alist of the GET parameters associated with the REQUEST
object REQUEST.

##### post-parameter

```lisp
Function: (post-parameter name &optional (request *request*))
```
Returns the POST parameter with name NAME (a string) - or NIL if
there is none.  Search is case-sensitive.

##### post-parameters\*

```lisp
Function: (post-parameters* &optional (request *request*))
```

Returns an alist of the POST parameters associated with the REQUEST
object REQUEST.

##### cookie-in

```lisp
Function: (cookie-in name &optional (request *request*))
```

Returns the cookie with the name NAME (a string) as sent by the
browser - or NIL if there is none.

##### cookies-in\*

```lisp
Function: (cookies-in* &optional (request *request*))
```

Returns an alist of all cookies associated with the REQUEST object
REQUEST.

##### host

```lisp
Function: (host &optional (request *request*))
```

Returns the 'Host' incoming http header value.

##### query-string\*

```lisp
Function: (query-string* &optional (request *request*))
```
Returns the query string of the REQUEST object REQUEST. That's
the part behind the question mark (i.e. the GET parameters).

##### referer

```lisp
Function: (referer &optional (request *request*))
```
Returns the 'Referer' (sic!) http header.

##### request-method\*

```lisp
Function: (request-method* &optional (request *request*))
```
Returns the request method as a Lisp keyword.


##### request-uri\*

```lisp
Function: (request-uri* &optional (request *request*))
```
Returns the request URI.

##### server-protocol\*

```lisp
Function: (server-protocol* &optional (request *request*))
```
Returns the request protocol as a Lisp keyword.

##### user-agent

```lisp
Function: (user-agent &optional (request *request*))
```
Returns the 'User-Agent' http header.

##### header-in\*

```lisp
Function: (header-in* name &optional (request *request*))
```
Returns the incoming header with name NAME.  NAME can be a keyword
(recommended) or a string.


##### headers-in\*

```lisp
Function: (headers-in* &optional (request *request*))
```
Returns an alist of the incoming headers associated with the
REQUEST object REQUEST.


##### remote-addr\*

```lisp
Function: (remote-addr* &optional (request *request*))
```
Returns the address the current request originated from.

##### remote-port\*

```lisp
Function: (remote-port* &optional (request *request*))
```
Returns the port the current request originated from.

##### local-addr\*

```lisp
Function: (local-addr* &optional (request *request*))
```
Returns the address the current request connected to.

##### local-port\*

```lisp
Function: (local-port* &optional (request *request*))
```
Returns the port the current request connected to.


##### script-name\*

```lisp
Function: (script-name* &optional (request *request*))
```
Returns the file name of the REQUEST object REQUEST. That's the
requested URI without the query string (i.e the GET parameters).

##### aux-request-value

```lisp
Function: (aux-request-value symbol &optional (request *request*))
```
Returns the value associated with SYMBOL from the request object
REQUEST (the default is the current request) if it exists.  The
second return value is true if such a value was found.
Sets the value associated with SYMBOL from the request object
REQUEST (default is \*REQUEST*).  If there is already a value
associated with SYMBOL it will be replaced.

##### delete-aux-request-value

```lisp
Function: (delete-aux-request-value symbol &optional (request *request*))
```
Removes the value associated with SYMBOL from the request object
REQUEST.

##### authorization

```lisp
Function: (authorization &optional (request *request*))
```
Returns as two values the user and password (if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header.

##### raw-post-data

```lisp
Function: (raw-post-data &key (request *request*) external-format force-text force-binary want-stream)
```
Returns the content sent by the client if there was any (unless
the content type was "multipart/form-data").  By default, the result
is a string if the type of the `Content-Type' media type is "text",
and a vector of octets otherwise.  In the case of a string, the
external format to be used to decode the content will be determined
from the `charset' parameter sent by the client (or otherwise
\*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT\* will be used).

You can also provide an external format explicitly (through
EXTERNAL-FORMAT) in which case the result will unconditionally be a
string.  Likewise, you can provide a true value for FORCE-TEXT which
will force Hunchentoot to act as if the type of the media type had
been "text".  Or you can provide a true value for FORCE-BINARY which
means that you want a vector of octets at any rate.

If, however, you provide a true value for WANT-STREAM, the other
parameters are ignored and you'll get the content (flexi) stream to
read from it yourself.  It is then your responsibility to read the
correct amount of data, because otherwise you won't be able to return
a response to the client.  If the content type of the request was
`multipart/form-data' or `application/x-www-form-urlencoded', the
content has been read by Hunchentoot already and you can't read from
the stream anymore.

You can call RAW-POST-DATA more than once per request, but you can't
mix calls which have different values for WANT-STREAM.

Note that this function is slightly misnamed because a client can send
content even if the request method is not POST.

##### recompute-request-parameters

```lisp
Function: (recompute-request-parameters 
             &key (request *request*) 
               (external-format *hunchentoot-default-external-format*))
```
Recomputes the GET and POST parameters for the REQUEST object
REQUEST.  This only makes sense if you're switching external formats
during the request.

##### process-request

```lisp
Function: (process-request request)
```
This function is called by PROCESS-CONNECTION after
the incoming headers have been read.  It calls HANDLE-REQUEST to
select and call a handler and sends the output of this handler to the
client using START-OUTPUT.  Note that PROCESS-CONNECTION is called
once per connection and loops in case of a persistent connection while
PROCESS-REQUEST is called anew for each request.

Essentially, you can view process-request as a thin wrapper around
HANDLE-REQUEST.

The return value of this function is ignored.


##### handle-request

```lisp
Function: (handle-request acceptor request)
```
This function is called once the request has been
read and a REQUEST object has been created.  Its job is to set up
standard error handling and request logging.

Might be a good place for around methods specialized for your subclass
of ACCEPTOR which bind or rebind special variables which can then be
accessed by your handlers.

##### handle-request

```lisp
Function: (handle-request acceptor request)
```
This function is called once the request has been
read and a REQUEST object has been created.  Its job is to set up
standard error handling and request logging.

Might be a good place for around methods specialized for your subclass
of ACCEPTOR which bind or rebind special variables which can then be
accessed by your handlers.

##### acceptor-dispatch-request

```lisp
Function: (acceptor-dispatch-request acceptor request)
```
This function is called to actually dispatch the
request once the standard logging and error handling has been set up.
ACCEPTOR subclasses implement methods for this function in order to
perform their own request routing.  If a method does not want to
handle the request, it is supposed to invoke CALL-NEXT-METHOD so that
the next ACCEPTOR in the inheritance chain gets a chance to handle the
request.

#### SLOTS

##### acceptor

```lisp
Initargs: :acceptor
Readers: hunchentoot:request-acceptor
```

 The acceptor which created this request
 object.
 
##### headers-in

```lisp
Initargs: :headers-in
Readers: hunchentoot:headers-in
```

 An alist of the incoming headers.

##### method

```lisp
Initargs: :method
Readers: hunchentoot:request-method
```

 The request method as a keyword.

##### uri

```lisp
Initargs: :uri
Readers: hunchentoot:request-uri
```

 The request URI as a string.

##### server-protocol

```lisp
Initargs: :server-protocol
Readers: hunchentoot:server-protocol
```

 The HTTP protocol as a keyword.

##### local-addr

```lisp
Initargs: :local-addr
Readers: hunchentoot:local-addr
```

 The IP address of the local system
 that the client connected to.

##### local-port

```lisp
Initargs: :local-port
Readers: hunchentoot:local-port
```

 The TCP port number of the local
 system that the client connected to.

##### remote-addr

```lisp
Initargs: :remote-addr
Readers: hunchentoot:remote-addr
```

 The IP address of the client that
 initiated this request.

##### remote-port

```lisp
Initargs: :remote-port
Readers: hunchentoot:remote-port
```

 The TCP port number of the client
 socket from which this request originated.

##### content-stream

```lisp
Initargs: :content-stream
```

 A stream from which the request
 body can be read if there is one.

##### cookies-in

```lisp
Readers: hunchentoot:cookies-in
```

 An alist of the cookies sent by the client.

##### get-parameters

```lisp
Readers: hunchentoot:get-parameters
```

 An alist of the GET parameters sent
 by the client.

##### post-parameters

```lisp
Readers: hunchentoot:post-parameters
```

 An alist of the POST parameters
 sent by the client.

##### script-name

```lisp
Readers: hunchentoot:script-name
```

 The URI requested by the client without
 the query string.

##### query-string

```lisp
Readers: hunchentoot:query-string
```

 The query string of this request.

##### session

```lisp
Readers: hunchentoot:session
Writers: (setf hunchentoot:session)
```

 The session object associated with this
 request.

##### aux-data

 Used to keep a user-modifiable alist with
 arbitrary data during the request.

##### raw-post-data

 The raw string sent as the body of a
 POST request, populated only if not a multipart/form-data request.
  
### 8. <span id="class-session">SESSION</span>


```lisp
Class
```

SESSION objects are automatically maintained by
Hunchentoot.  They should not be created explicitly with MAKE-INSTANCE
but implicitly with START-SESSION and they should be treated as opaque
objects.

You can ignore Hunchentoot's SESSION objects altogether and implement
your own sessions if you provide corresponding methods for
SESSION-COOKIE-VALUE and SESSION-VERIFY.

-  Direct superclasses: STANDARD-OBJECT
-  No subclasses.

Hunchentoot supports *sessions*: Once a [request
handler](#request-dispatch) has called `START-SESSION`,
Hunchentoot uses either cookies or (if the client doesn't send the
cookies back) [rewrites URLs](#*rewrite-for-session-urls*) to keep track
of this client, i.e. to provide a kind of 'state' for the stateless http
protocol. 

Hunchentoot makes some reasonable effort to prevent eavesdroppers from
hijacking sessions (see below), but this should not be considered really
secure. Don't store sensitive data in sessions and rely solely on the
session mechanism as a safeguard against malicious users who want to get
at this data!

For each request there's one `SESSION` object which is
accessible to the [handler](#handler) via the special variable
`*SESSION*`. This object holds all the information available
about the session and can be accessed with the functions described in
this chapter. Note that the internal structure of `SESSION`
objects should be considered opaque and may change in future releases of
Hunchentoot.

Sessions are automatically [verified](#session-verify) for validity and
age when the `REQUEST` object is instantiated, i.e. if
`*SESSION*` is not NIL then this session is valid (as far as
Hunchentoot is concerned) and not [too old](#session-too-old-p). Old
sessions are [automatically removed](#session-gc).

Hunchentoot also provides a `SESSION-REGENERATE-COOKIE-VALUE`
function that creates a new cookie value. This helps to prevent against
[session fixation
attacks](https://www.owasp.org/index.php/Session_fixation), and should
be used when a user logs in according to the application.


#### RELEVANT METHODS

##### start-session

```lisp
Function: (start-session)
```
Returns the current SESSION object. If there is no current session,
creates one and updates the corresponding data structures. In this
case the function will also send a session cookie to the browser.

##### session-value

```lisp
Function: (session-value symbol &optional (session *session*))
```
Returns the value associated with SYMBOL from the session object
SESSION (the default is the current session) if it exists.
Sets the value associated with SYMBOL from the session object
SESSION. If there is already a value associated with SYMBOL it will be
replaced. Will automatically start a session if none was supplied and
there's no session for the current request.

##### delete-session-value

```lisp
Function: (delete-session-value symbol &optional (session *session*))
```
Removes the value associated with SYMBOL from SESSION if there is
one.

##### remove-session

```lisp
Function: (remove-session session)
```
Completely removes the SESSION object SESSION from Hunchentoot's
internal session database.



#### SLOTS

##### session-id

```lisp
Type: integer
Initform: (hunchentoot:next-session-id (hunchentoot:request-acceptor hunchentoot:*request*))
Readers: hunchentoot:session-id
```

 The unique ID (an INTEGER) of the session.

##### session-string

 The session string encodes enough
 data to safely retrieve this session.  It is sent to the browser as a
 cookie value or as a GET parameter.

##### user-agent

```lisp
Initform: (hunchentoot:user-agent hunchentoot:*request*)
Readers: hunchentoot:session-user-agent
```

 The incoming 'User-Agent' header that
 was sent when this session was created.

##### remote-addr

```lisp
Initform: (hunchentoot:real-remote-addr hunchentoot:*request*)
Readers: hunchentoot:session-remote-addr
```

 The remote IP address of the client
 when this session was started as returned by REAL-REMOTE-ADDR.
  
##### session-start
```lisp
Initform: (GET-UNIVERSAL-TIME)
Readers: hunchentoot:session-start
```

 The time this session was started.

##### last-click

 The last time this session was used.

##### session-data

```lisp
Initargs: :session-data
```

 Data associated with this session -
 see SESSION-VALUE.

##### max-time

```lisp
Type: fixnum
Initargs: :max-time
Initform: hunchentoot:*session-max-time*
Readers: hunchentoot:session-max-time
Writers: (setf hunchentoot:session-max-time)
```

 The time (in seconds) after which this
 session expires if it's not used.

##### session-id

```lisp
Type: integer
Initform: (hunchentoot:next-session-id (hunchentoot:request-acceptor hunchentoot:*request*))
Readers: hunchentoot:session-id
```

 The unique ID (an INTEGER) of the session.

##### session-string

 The session string encodes enough
 data to safely retrieve this session.  It is sent to the browser as a
 cookie value or as a GET parameter.
 
##### user-agent

```lisp
Initform: (HUNCHENTOOT:USER-AGENT HUNCHENTOOT:*REQUEST*)
Readers: hunchentoot:session-user-agent
```

 The incoming 'User-Agent' header that
 was sent when this session was created.
 
##### remote-addr

```lisp
Initform: (hunchentoot:real-remote-addr hunchentoot:*request*)
Readers: hunchentoot:session-remote-addr
```

 The remote IP address of the client
 when this session was started as returned by REAL-REMOTE-ADDR.

##### session-start

```lisp
Initform: (get-universal-time)
Readers: hunchentoot:session-start
```

 The time this session was started.

### 9. MISCELLANEOUS FUNCTIONS

#### bad-request

```lisp
Class
```

-  Direct superclasses: [HUNCHENTOOT-ERROR](#hunchentoot-error)
-  No subclasses.


#### client-as-string

```lisp
Function: (client-as-string socket)
```
A helper function which returns the client's address and port as a
string and tries to act robustly in the presence of network problems.

#### create-folder-dispatcher-and-handler

```lisp
Function: (create-folder-dispatcher-and-handler uri-prefix base-path &optional content-type)
```
Creates and returns a dispatch function which will dispatch to a
handler function which emits the file relative to BASE-PATH that is
denoted by the URI of the request relative to URI-PREFIX.  URI-PREFIX
must be a string ending with a slash, BASE-PATH must be a pathname
designator for an existing directory.  If CONTENT-TYPE is not NIL,
it'll be the content type used for all files in the folder.

#### create-prefix-dispatcher

```lisp
Function: (create-prefix-dispatcher prefix handler)
```
Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
starts with the string PREFIX.

#### create-regex-dispatcher

```lisp
Function: (create-regex-dispatcher regex handler)
```
Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
matches the CL-PPCRE regular expression REGEX.

#### create-request-handler-thread

```lisp
Function: (create-request-handler-thread taskmaster socket)
```
Create a new thread in which to process the request.
 This thread will call PROCESS-CONNECTION to process the request.

#### create-static-file-dispatcher-and-handler

```lisp
Function: (create-static-file-dispatcher-and-handler uri path &optional content-type)
```
Creates and returns a request dispatch function which will dispatch
to a handler function which emits the file denoted by the pathname
designator PATH with content type CONTENT-TYPE if the SCRIPT-NAME of
the request matches the string URI.  If CONTENT-TYPE is NIL, tries to
determine the content type via the file's suffix.

#### decrement-taskmaster-thread-count

```lisp
Function: (decrement-taskmaster-thread-count taskmaster)
```
Atomically decrement the number of taskmaster requests

#### default-document-directory

```lisp
Function: (default-document-directory &optional sub-directory)
```

#### detach-socket

```lisp
Function: (detach-socket acceptor)
```
Indicate to Hunchentoot that it should stop serving
 requests on the current request's socket.
 Hunchentoot will finish processing the current
 request and then return from PROCESS-CONNECTION
 without closing the connection to the client.
 DETACH-SOCKET can only be called from within a
 request handler function.

#### dispatch-easy-handlers

```lisp
Function: (dispatch-easy-handlers request)
```
This is a dispatcher which returns the appropriate handler
defined with DEFINE-EASY-HANDLER, if there is one.

#### escape-for-html

```lisp
Function: (escape-for-html string)
```
Escapes the characters #\<, #\>, #\', #\", and #\& for HTML
output.

#### handle-if-modified-since

```lisp
Function: (handle-if-modified-since time &optional (request *request*))
```
Handles the 'If-Modified-Since' header of REQUEST.  The date string
is compared to the one generated from the supplied universal time
TIME.

#### handle-static-file

```lisp
Function: (handle-static-file pathname &optional content-type)
```
A function which acts like a Hunchentoot handler for the file
denoted by PATHNAME.  Sends a content type header corresponding to
CONTENT-TYPE or (if that is NIL) tries to determine the content type
via the file's suffix.

#### http-token-p

```lisp
Function: (http-token-p token)
```
This function tests whether OBJECT is a non-empty string which is a
TOKEN according to RFC 2068 (i.e. whether it may be used for, say,
cookie names).


#### hunchentoot-error

```lisp
Function: (hunchentoot-error format-control &rest format-arguments)
```
Signals an error of type HUNCHENTOOT-SIMPLE-ERROR with the provided
format control and arguments.
Superclass for all errors related to Hunchentoot.


#### increment-taskmaster-thread-count

```lisp
Function: (increment-taskmaster-thread-count taskmaster)
```
Atomically increment the number of taskmaster requests.

#### initialize-connection-stream

```lisp
Function: (initialize-connection-stream acceptor stream)
```
Can be used to modify the stream which is used to
communicate between client and server before the request is read.  The
default method of ACCEPTOR does nothing, but see for example the
method defined for SSL-ACCEPTOR.  All methods of this generic function
must return the stream to use.


#### log-message\*

```lisp
Function: (log-message* log-level format-string &rest format-arguments)
```
Convenience function which calls the message logger of the current
acceptor (if there is one) with the same arguments it accepts.

This is the function which Hunchentoot itself uses to log errors it
catches during request processing.

#### maybe-invoke-debugger

```lisp
Function: (maybe-invoke-debugger condition)
```
This generic function is called whenever a
condition CONDITION is signaled in Hunchentoot.  You might want to
specialize it on specific condition classes for debugging purposes.

#### mime-type

```lisp
Function: (mime-type pathspec)
```
Given a pathname designator PATHSPEC returns the MIME type
(as a string) corresponding to the suffix of the file denoted by
PATHSPEC (or NIL).

#### next-session-id

```lisp
Function: (next-session-id acceptor)
```
Returns the next sequential session ID, an integer,
which should be unique per session.  The default method uses a simple
global counter and isn't guarded by a lock.  For a high-performance
production environment you might consider using a more robust
implementation.

#### no-cache

```lisp
Function: (no-cache)
```
Adds appropriate headers to completely prevent caching on most browsers.

#### parameter-error

```lisp
Function: (parameter-error format-control &rest format-arguments)
```
Signals an error of type PARAMETER-ERROR with the provided
format control and arguments.
Signalled if a function was called with incosistent or illegal parameters.

#### process-connection

```lisp
Function: (process-connection acceptor socket)
```
This function is called by the taskmaster when a
new client connection has been established.  Its arguments are the
ACCEPTOR object and a LispWorks socket handle or a usocket socket
stream object in SOCKET.  It reads the request headers, sets up the
request and reply objects, and hands over to PROCESS-REQUEST.  This is
done in a loop until the stream has to be closed or until a connection
timeout occurs.

It is probably not a good idea to re-implement this method until you
really, really know what you're doing.

#### reason-phrase

```lisp
Function: (reason-phrase return-code)
```
Returns a reason phrase for the HTTP return code RETURN-CODE (which
should be an integer) or NIL for return codes Hunchentoot doesn't know.

#### redirect

```lisp
Function: (redirect target &key 
                             (host (host *request*) host-provided-p)
                             port 
                             (protocol (if (ssl-p) :https :http))
                             (add-session-id (not (or host-provided-p
                                                      (starts-with-scheme-p target)
                                                      (cookie-in (session-cookie-name *acceptor*)))))
                             (code +http-moved-temporarily+))
```
Redirects the browser to TARGET which should be a string.  If
TARGET is a full URL starting with a scheme, HOST, PORT and PROTOCOL
are ignored.  Otherwise, TARGET should denote the path part of a URL,
PROTOCOL must be one of the keywords :HTTP or :HTTPS, and the URL to
redirect to will be constructed from HOST, PORT, PROTOCOL, and TARGET.
Adds a session ID if ADD-SESSION-ID is true.  If CODE is a 3xx
redirection code, it will be sent as status code.

#### regenerate-session-cookie-value

```lisp
Function: (regenerate-session-cookie-value session)
```
Regenerates the cookie value. This should be used
when a user logs in according to the application to prevent against
session fixation attacks. The cookie value being dependent on ID,
USER-AGENT, REMOTE-ADDR, START, and \*SESSION-SECRET*, the only value
we can change is START to regenerate a new value. Since we're
generating a new cookie, it makes sense to have the session being
restarted, in time. That said, because of this fact, calling this
function twice in the same second will regenerate twice the same value.

#### reply-external-format\*

```lisp
Function: (reply-external-format* &optional (reply *reply*))
```
The external format of REPLY which is used for character output.

#### request-pathname

```lisp
Function: (request-pathname &optional (request *request*) drop-prefix)
```
Construct a relative pathname from the request's SCRIPT-NAME.
If DROP-PREFIX is given, pathname construction starts at the first path
segment after the prefix.

#### require-authorization

```lisp
Function: (require-authorization &optional (realm hunchentoot))
```
Sends back appropriate headers to require basic HTTP authentication
(see RFC 2617) for the realm REALM.

#### reset-connection-stream

```lisp
Function: (reset-connection-stream acceptor stream)
```
Resets the stream which is used to communicate
between client and server after one request has been served so that it
can be used to process the next request.  This generic function is
called after a request has been processed and must return the
stream.

#### reset-session-secret

```lisp
Function: (reset-session-secret)
```
Sets \*SESSION-SECRET\* to a new random value. All old sessions will
cease to be valid.

#### reset-sessions

```lisp
Function: (reset-sessions &optional (acceptor *acceptor*))
```
Removes ALL stored sessions of ACCEPTOR.

#### rfc-1123-date

```lisp
Function: (rfc-1123-date &optional (time (get-universal-time)))
```
Generates a time string according to RFC 1123. Default is current time.
This can be used to send a 'Last-Modified' header - see
HANDLE-IF-MODIFIED-SINCE.

#### script-name

```lisp
Function: (script-name object)
```
#### server-protocol

```lisp
Function: (server-protocol object)
```


#### session-cookie-name

```lisp
Function: (session-cookie-name acceptor)
```
Returns the name (a string) of the cookie (or the
GET parameter) which is used to store a session on the client side.
The default is to use the string "hunchentoot-session", but you can
specialize this function if you want another name.

#### session-cookie-value

```lisp
Function: (session-cookie-value session)
```
Returns a string which can be used to safely
restore the session SESSION if as session has already been
established.  This is used as the value stored in the session cookie
or in the corresponding GET parameter and verified by SESSION-VERIFY.

A default method is provided and there's no reason to change it unless
you want to use your own session objects.

#### session-created

```lisp
Function: (session-created acceptor new-session)
```
This function is called whenever a new session has
been created.  There's a default method which might trigger a session
GC based on the value of \*SESSION-GC-FREQUENCY\*.

The return value is ignored.

#### session-db

```lisp
Function: (session-db acceptor)
```
Returns the current session database which is an
alist where each car is a session's ID and the cdr is the
corresponding SESSION object itself.  The default is to use a global
list for all acceptors.

#### session-db-lock

```lisp
Function: (session-db-lock acceptor &key whole-db-p)
```
A function which returns a lock that will be used
to prevent concurrent access to sessions.  The first argument will be
the acceptor that handles the current request, the second argument is
true if the whole (current) session database is modified.  If it is
NIL, only one existing session in the database is modified.

This function can return NIL which means that sessions or session
databases will be modified without a lock held (for example for
single-threaded environments).  The default is to always return a
global lock (ignoring the ACCEPTOR argument) for Lisps that support
threads and NIL otherwise.

#### session-gc

```lisp
Function: (session-gc)
```
Removes sessions from the current session database which are too
old - see SESSION-TOO-OLD-P.

#### session-too-old-p

```lisp
Function: (session-too-old-p session)
```
Returns true if the SESSION object SESSION has not been active in
the last (SESSION-MAX-TIME SESSION) seconds.

#### session-verify

```lisp
Function: (session-verify request)
```
Tries to get a session identifier from the cookies
(or alternatively from the GET parameters) sent by the client (see
SESSION-COOKIE-NAME and SESSION-COOKIE-VALUE).  This identifier is
then checked for validity against the REQUEST object REQUEST.  On
success the corresponding session object (if not too old) is returned
(and updated).  Otherwise NIL is returned.

A default method is provided and you only need to write your own one
if you want to maintain your own sessions.

#### set-cookie

```lisp
Function: (set-cookie name &key (value "") expires max-age 
                             path domain secure http-only (reply *reply*))
```
Creates a cookie object from the parameters provided and adds
it to the outgoing cookies of the REPLY object REPLY. If a cookie
with the name NAME (case-sensitive) already exists, it is
replaced.

#### set-cookie\*

```lisp
Function: (set-cookie* cookie &optional (reply *reply*))
```
Adds the COOKIE object COOKIE to the outgoing cookies of the
REPLY object REPLY. If a cookie with the same name
(case-sensitive) already exists, it is replaced.


#### ssl-p

```lisp
Function: (ssl-p &optional (acceptor *acceptor*))
```
Whether the current connection to the client is secure. See
ACCEPTOR-SSL-P.



#### taskmaster-max-accept-count

```lisp
Function: (taskmaster-max-accept-count taskmaster)
```
The maximum number of connections this taskmaster will accept before refusing
    new connections.  If supplied, this must be greater than MAX-THREAD-COUNT.
    The number of queued requests is the difference between MAX-ACCEPT-COUNT
    and MAX-THREAD-COUNT.

#### taskmaster-max-thread-count

```lisp
Function: (taskmaster-max-thread-count taskmaster)
```
The maximum number of request threads this taskmaster will simultaneously
    run before refusing or queueing new connections requests.  If the value
    is null, then there is no limit.

#### taskmaster-thread-count

```lisp
Function: (taskmaster-thread-count taskmaster)
```
Returns the current number of taskmaster requests.

#### too-many-taskmaster-requests

```lisp
Function: (too-many-taskmaster-requests taskmaster socket)
```
Signal a "too many requests" error, just prior to closing the connection.

#### url-decode

```lisp
Function: (url-decode string &optional (external-format
                                        *hunchentoot-default-external-format*))
```
Decodes a URL-encoded string which is assumed to be encoded using the
external format EXTERNAL-FORMAT, i.e. this is the inverse of
URL-ENCODE. It is assumed that you'll rarely need this function, if
ever. But just in case - here it is. The default for EXTERNAL-FORMAT is
the value of \*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT\*.

#### url-encode

```lisp
Function: (url-encode string &optional (external-format
                                        *hunchentoot-default-external-format*))
```
URL-encodes a string using the external format EXTERNAL-FORMAT. The
default for EXTERNAL-FORMAT is the value of
\*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT\*.

#### within-request-p

```lisp
Function: (within-request-p)
```
True if we're in the context of a request, otherwise nil.


### 10. <span id="configuration-variables">CONFIGURATION VARIABLES</span>

#### \*catch-errors-p\*

```lisp
Variable
```

Whether Hunchentoot should catch and log errors (or rather invoke
the debugger).

#### \*content-types-for-url-rewrite\*

```lisp
Variable
```

The content types for which url-rewriting is OK. See
[\*REWRITE-FOR-SESSION-URLS\*](#rewrite-for-session-urls).

#### \*default-connection-timeout\*

```lisp
Variable
```

The default connection timeout used when an acceptor is reading
from and writing to a socket stream.

#### \*default-content-type\*

```lisp
Variable
```

The default content-type header which is returned to the client.
If this is text content type, the character set used for encoding the
response will automatically be added to the content type in a
``charset'' attribute.

#### \*dispatch-table\*

```lisp
Variable
```

A global list of dispatch functions.

#### \*file-upload-hook\*

```lisp
Variable
```

If this is not NIL, it should be a unary function which will
be called with a pathname for each file which is uploaded to
Hunchentoot.  The pathname denotes the temporary file to which
the uploaded file is written.  The hook is called directly before
the file is created.

#### \*handle-http-errors-p\*

#### \*header-stream\*

```lisp
Variable
```

If this variable is not NIL, it should be bound to a stream to
which incoming and outgoing headers will be written for debugging
purposes.

#### \*http-error-handler\*

#### \*hunchentoot-default-external-format\*

```lisp
Variable
```

The external format used to compute the REQUEST object.

#### \*hunchentoot-version\*

#### \*lisp-errors-log-level\*

```lisp
Variable
```

Log level for Lisp errors.  Should be one of :ERROR (the default),
:WARNING, or :INFO.

#### \*lisp-warnings-log-level\*

```lisp
Variable
```

Log level for Lisp warnings.  Should be one of :ERROR, :WARNING
(the default), or :INFO.

#### \*log-lisp-backtraces-p\*

```lisp
Variable
```

Whether Lisp backtraces should be logged.  Only has an effect if
[\*LOG-LISP-ERRORS-P\*](#log-lisp-errors-p] is true as well.

#### \*log-lisp-errors-p\*

```lisp
Variable
```

Whether Lisp errors in request handlers should be logged.

#### \*log-lisp-warnings-p\*

```lisp
Variable
```

Whether Lisp warnings in request handlers should be logged.

#### \*methods-for-post-parameters\*

```lisp
Variable
```

A list of the request method types (as keywords) for which
Hunchentoot will try to compute [POST-PARAMETERS](#post-parameters).

#### \*rewrite-for-session-urls\*

```lisp
Variable
```

Whether HTML pages should possibly be rewritten for cookie-less
session-management.

#### \*session-gc-frequency\*

```lisp
Variable
```

A session GC (see function SESSION-GC) will happen every
\*SESSION-GC-FREQUENCY* requests (counting only requests which create
a new session) if this variable is not NIL.  See [SESSION-CREATED](#session-created).

#### \*session-max-time\*

```lisp
Variable
```

The default time (in seconds) after which a session times out.

#### \*session-secret\*

```lisp
Variable
```

A random ASCII string that's used to encode the public session
data.  This variable is initially unbound and will be set (using
[RESET-SESSION-SECRET](#reset-session-secret)) the first time a session is created, if
necessary.  You can prevent this from happening if you set the value
yourself before starting acceptors.

#### \*show-lisp-backtraces-p\*

```lisp
Variable
```

Whether Lisp errors shown in HTML output should contain backtrace information.

#### \*show-lisp-errors-p\*

```lisp
Variable
```

Whether Lisp errors in request handlers should be shown in HTML output.

#### \*tmp-directory\*

```lisp
Variable
```

Directory for temporary files created by MAKE-TMP-FILE-NAME.

#### \*use-remote-addr-for-sessions\*

```lisp
Variable
```

Whether the client's remote IP (as returned by [REAL-REMOTE-ADDR](#real-remote-addr))
should be encoded into the session string.  If this value is true, a
session will cease to be accessible if the client's remote IP changes.

This might for example be an issue if the client uses a proxy server
which doesn't send correct 'X_FORWARDED_FOR' headers.

#### \*use-user-agent-for-sessions\*

```lisp
Variable
```

Whether the 'User-Agent' header should be encoded into the session
string.  If this value is true, a session will cease to be accessible
if the client sends a different 'User-Agent' header.

### 11. <span id="variables-in-request-context">VARIABLES IN THE CONTEXT OF A REQUEST</span>

#### \*acceptor\*

```lisp
Variable

```
The current [ACCEPTOR](#acceptor) object.

#### \*reply\*

```lisp
Variable

```
The current [REPLY](#class-reply) object.

#### \*request\*

```lisp
Variable

```
The current [REQUEST](#class-request) object.

#### \*session\*

```lisp
Variable
```

The current [SESSION](#class-session) (can be NIL).

### 12. CONSTANTS

#### +http-accepted+

```lisp
Constant: 202
```

HTTP return code (202) for 'Accepted'.

#### +http-authorization-required+

```lisp
Constant: 401
```

HTTP return code (401) for 'Authorization Required'.

#### +http-bad-gateway+

```lisp
Constant: 502
```

HTTP return code (502) for 'Bad Gateway'.

#### +http-bad-request+

```lisp
Constant: 400
```

HTTP return code (400) for 'Bad Request'.

#### +http-conflict+

```lisp
Constant: 409
```

HTTP return code (409) for 'Conflict'.

#### +http-continue+

```lisp
Constant: 100
```

HTTP return code (100) for 'Continue'.

#### +http-created+

```lisp
Constant: 201
```

HTTP return code (201) for 'Created'.

#### +http-expectation-failed+

```lisp
Constant: 417
```

HTTP return code (417) for 'Expectation Failed'.

#### +http-failed-dependency+

```lisp
Constant: 424
```

HTTP return code (424) for 'Failed Dependency'.

#### +http-forbidden+

```lisp
Constant: 403
```

HTTP return code (403) for 'Forbidden'.

#### +http-gateway-time-out+

```lisp
Constant: 504
```

HTTP return code (504) for 'Gateway Time-out'.

#### +http-gone+

```lisp
Constant: 410
```

HTTP return code (410) for 'Gone'.

#### +http-internal-server-error+

```lisp
Constant: 500
```

HTTP return code (500) for 'Internal Server Error'.

#### +http-length-required+

```lisp
Constant: 411
```

HTTP return code (411) for 'Length Required'.

#### +http-method-not-allowed+

```lisp
Constant: 405
```

HTTP return code (405) for 'Method Not Allowed'.

#### +http-moved-permanently+

```lisp
Constant: 301
```

HTTP return code (301) for 'Moved Permanently'.

#### +http-moved-temporarily+

```lisp
Constant: 302
```

HTTP return code (302) for 'Moved Temporarily'.

#### +http-multi-status+

```lisp
Constant: 207
```

HTTP return code (207) for 'Multi-Status'.

#### +http-multiple-choices+

```lisp
Constant: 300
```

HTTP return code (300) for 'Multiple Choices'.

#### +http-network-authentication-required+

```lisp
Constant: 511
```

HTTP return code (511) for 'Network Authentication Required'.

#### +http-no-content+

```lisp
Constant: 204
```

HTTP return code (204) for 'No Content'.

#### +http-non-authoritative-information+

```lisp
Constant: 203
```

HTTP return code (203) for 'Non-Authoritative Information'.

#### +http-not-acceptable+

```lisp
Constant: 406
```

HTTP return code (406) for 'Not Acceptable'.

#### +http-not-found+

```lisp
Constant: 404
```

HTTP return code (404) for 'Not Found'.

#### +http-not-implemented+

```lisp
Constant: 501
```

HTTP return code (501) for 'Not Implemented'.

#### +http-not-modified+

```lisp
Constant: 304
```

HTTP return code (304) for 'Not Modified'.

#### +http-ok+

```lisp
Constant: 200
```

HTTP return code (200) for 'OK'.

#### +http-partial-content+

```lisp
Constant: 206
```

HTTP return code (206) for 'Partial Content'.

#### +http-payment-required+

```lisp
Constant: 402
```

HTTP return code (402) for 'Payment Required'.

#### +http-precondition-failed+

```lisp
Constant: 412
```

HTTP return code (412) for 'Precondition Failed'.

#### +http-precondition-required+

```lisp
Constant: 428
```

HTTP return code (428) for 'Precondition Required'.

#### +http-proxy-authentication-required+

```lisp
Constant: 407
```

HTTP return code (407) for 'Proxy Authentication Required'.

#### +http-request-entity-too-large+

```lisp
Constant: 413
```

HTTP return code (413) for 'Request Entity Too Large'.

#### +http-request-header-fields-too-large+

```lisp
Constant: 431
```

HTTP return code (431) for 'Request Header Fields Too Large'.

#### +http-request-time-out+

```lisp
Constant: 408
```

HTTP return code (408) for 'Request Time-out'.

#### +http-request-uri-too-large+

```lisp
Constant: 414
```

HTTP return code (414) for 'Request-URI Too Large'.

#### +http-requested-range-not-satisfiable+

```lisp
Constant: 416
```

HTTP return code (416) for 'Requested range not satisfiable'.

#### +http-reset-content+

```lisp
Constant: 205
```

HTTP return code (205) for 'Reset Content'.

#### +http-see-other+

```lisp
Constant: 303
```

HTTP return code (303) for 'See Other'.

#### +http-service-unavailable+

```lisp
Constant: 503
```

HTTP return code (503) for 'Service Unavailable'.

#### +http-switching-protocols+

```lisp
Constant: 101
```

HTTP return code (101) for 'Switching Protocols'.

#### +http-temporary-redirect+

```lisp
Constant: 307
```

HTTP return code (307) for 'Temporary Redirect'.

#### +http-too-many-requests+

```lisp
Constant: 429
```

HTTP return code (429) for 'Too Many Requests'.

#### +http-unsupported-media-type+

```lisp
Constant: 415
```

HTTP return code (415) for 'Unsupported Media Type'.

#### +http-use-proxy+

```lisp
Constant: 305
```

HTTP return code (305) for 'Use Proxy'.

#### +http-version-not-supported+

```lisp
Constant: 505
```

HTTP return code (505) for 'Version not supported'.

### 13. HUNCHENTOOT-CONDITION

```lisp
Class
```

Superclass for all conditions related to Hunchentoot.


### 14. HUNCHENTOOT-WARNING

```lisp
Class
```

Superclass for all warnings related to Hunchentoot.

### 15. TASKMASTER

#### <span id="class-taskmaster">taskmaster</span>

```lisp
Class
```

An instance of this class is responsible for
distributing the work of handling requests for its acceptor.  This is
an "abstract" class in the sense that usually only instances of
subclasses of TASKMASTER will be used.

- Direct superclasses: STANDARD-OBJECT
- Direct subclasses: [MULTI-THREADED-TASKMASTER](#multi-threaded-taskmaster), [SINGLE-THREADED-TASKMASTER](#single-threaded-taskmaster)

##### RELEVANT METHODS

###### shutdown

```lisp
Function: (shutdown taskmaster)
```
Shuts down the taskmaster, i.e. frees all resources
that were set up by it.  For example, a multi-threaded taskmaster
might terminate all threads that are currently associated with it.
This function is called by the acceptor's STOP method.

###### execute-acceptor

```lisp
Function: (execute-acceptor taskmaster)
```
This is a callback called by the acceptor once it
has performed all initial processing to start listening for incoming
connections (see START-LISTENING).  It usually calls the
ACCEPT-CONNECTIONS method of the acceptor, but depending on the
taskmaster instance the method might be called from a new thread.

###### handle-incoming-connection

```lisp
Function: (handle-incoming-connection taskmaster socket)
```
This function is called by the acceptor to start
processing of requests on a new incoming connection.  SOCKET is the
usocket instance that represents the new connection (or a socket
handle on LispWorks).  The taskmaster starts processing requests on
the incoming connection by calling the PROCESS-CONNECTION method of
the acceptor instance.  The SOCKET argument is passed to
PROCESS-CONNECTION as an argument.

###### start-thread

```lisp
Function: (start-thread taskmaster thunk &key name)
```
Start a name thread in which to call the THUNK, in the context of the given TASKMASTER.
Keyword arguments provide TASKMASTER-dependent options.
Return a thread object.

Hunchentoot taskmaster methods will call it with the taskmaster as the context,
allowing hunchentoot extensions to define specialized methods that may e.g.
wrap the thunk within a proper set of bindings and condition handlers.


##### DIRECT SLOTS

###### acceptor
```lisp
Readers: taskmaster-acceptor
Writers: (setf taskmaster-acceptor)
```

A backpointer to the acceptor instance
this taskmaster works for.

#### multi-threaded-taskmaster

```lisp
Class
```

An abstract class for taskmasters that use multiple threads.
For a concrete class to instantiate, use one-thread-per-connection-taskmaster.

-  Direct superclasses: [TASKMASTER](#class-taskmaster)
-  Direct subclasses: [ONE-THREAD-PER-CONNECTION-TASKMASTER](#one-thread-per-connection-taskmaster)


##### DIRECT SLOTS

###### acceptor-process

 A process that accepts incoming connections and hands them off to new processes
  for request handling.
  
#### one-thread-per-connection-taskmaster

```lisp
Class
```

A taskmaster that starts one thread for listening
to incoming requests and one new thread for each incoming connection.

If MAX-THREAD-COUNT is null, a new thread will always be created for
each request.

If MAX-THREAD-COUNT is supplied, the number of request threads is
limited to that.  Furthermore, if MAX-ACCEPT-COUNT is not supplied, an
HTTP 503 will be sent if the thread limit is exceeded.  Otherwise, if
MAX-ACCEPT-COUNT is supplied, it must be greater than MAX-THREAD-COUNT;
in this case, requests are accepted up to MAX-ACCEPT-COUNT, and only
then is HTTP 503 sent.

It is important to note that MAX-ACCEPT-COUNT and the HTTP 503 behavior
described above is racing with the acceptor listen backlog. If we are receiving
requests faster than threads can be spawned and 503 sent, the requests will be
silently rejected by the kernel.

In a load-balanced environment with multiple Hunchentoot servers, it's
reasonable to provide MAX-THREAD-COUNT but leave MAX-ACCEPT-COUNT null.
This will immediately result in HTTP 503 when one server is out of
resources, so the load balancer can try to find another server.

In an environment with a single Hunchentoot server, it's reasonable
to provide both MAX-THREAD-COUNT and a somewhat larger value for
MAX-ACCEPT-COUNT.  This will cause a server that's almost out of
resources to wait a bit; if the server is completely out of resources,
then the reply will be HTTP 503.

This is the default taskmaster implementation for multi-threaded Lisp
implementations.

-  Direct superclasses: [MULTI-THREADED-TASKMASTER](#multi-threaded-taskmaster)
-  No subclasses.

##### DIRECT SLOTS

###### max-thread-count

```lisp
Type: (OR INTEGER NULL)
Initargs: :max-thread-count
Readers: taskmaster-max-thread-count
Writers: (setf taskmaster-max-thread-count)
```

 The maximum number of request threads this taskmaster will simultaneously
  run before refusing or queueing new connections requests.  If the value
  is null, then there is no limit.

###### thread-count

```lisp
Type: INTEGER
Initform: 0
Readers: taskmaster-thread-count
Writers: (setf taskmaster-thread-count)
```

 The number of taskmaster processing threads currently running.

###### thread-count-lock

 In the absence of 'atomic-incf', we need this to atomically
  increment and decrement the request count.

###### max-accept-count

```lisp
Type: (OR INTEGER NULL)
Initargs: :max-accept-count
Readers: taskmaster-max-accept-count
Writers: (setf taskmaster-max-accept-count)
```

 The maximum number of connections this taskmaster will accept before refusing
  new connections.  If supplied, this must be greater than MAX-THREAD-COUNT.
  The number of queued requests is the difference between MAX-ACCEPT-COUNT
  and MAX-THREAD-COUNT.

###### accept-count

```lisp
Type: INTEGER
Initform: 0
```

 The number of connection currently accepted by the taskmaster. These
 connections are not ensured to be processed, thay may be waiting for an
 empty processing slot or rejected because the load is too heavy.

###### accept-count-lock

 In the absence of 'atomic-incf', we need this to atomically
  increment and decrement the accept count.

###### wait-queue

 A queue that we use to wait for a free connection.

###### wait-lock

 The lock for the connection wait queue.

###### worker-thread-name-format

```lisp
Type: (OR STRING NULL)
Initargs: :worker-thread-name-format
Initform: "hunchentoot-worker-~A"
```

#### single-threaded-taskmaster

```lisp
Class
```

A taskmaster that runs synchronously in the thread
where the START function was invoked (or in the case of LispWorks in
the thread started by COMM:START-UP-SERVER).  This is the simplest
possible taskmaster implementation in that its methods do nothing but
calling their acceptor "sister" methods - EXECUTE-ACCEPTOR calls
ACCEPT-CONNECTIONS, HANDLE-INCOMING-CONNECTION calls
PROCESS-CONNECTION.

- Direct superclasses: [TASKMASTER](#class-taskmaster)

## ADVANCED TOPICS

### CUSTOMIZING SESSION BEHAVIOUR

For everyday session usage, you will probably just use
`START-SESSION`, `SESSION-VALUE`, and maybe
`DELETE-SESSION-VALUE` and `*SESSION*`. However,
there are two ways to customize the way Hunchentoot maintains sessions.

One way is to mostly leave the session mechanism intact but to tweak it
a bit:

-   The publicly visible part of a session is encoded using a
    [secret](#session-secret) which you can set yourself.
-   And it is stored using a cookie (or GET parameter)
    [name](#session-cookie-name) that you can override.
-   Each session receives a [new ID](#next-session-id) when it is
    created and you can implement a more robust way to do that.
-   You can arrange to be called whenever a session is
    [created](#session-created) to trigger some action. You might also
    do this to invent your own session [garbage
    collection](#session-gc).
-   By default, all sessions are stored in a global alist in memory. You
    can't change the alist part, but you can distribute your sessions
    over different ["databases"](#session-db).
-   By default, every operation which modifies sessions or one of the
    session databases is guarded by a global lock, but you can arrange
    to [provide](#session-db-lock) different locks for this.

The other way to customize Hunchentoot's sessions is to completely
replace them. This is actually pretty easy: Create your own class to
store state (which doesn't have to and probably shouldn't inherit from
`SESSION`) and implement methods for
`SESSION-VERIFY` and `SESSION-COOKIE-VALUE` - that's
it. Hunchentoot will continue to use cookies and/or to rewrite URLs to
keep track of session state and it will store "the current session"
(whatever that is in your implementation) in `*SESSION*`.
Everything else (like persisting sessions, GC, getting and setting
values) you'll have to take care of yourself and the other session
functions (like `START-SESSION` or `SESSION-VALUE`)
won't work anymore. (Almost) total freedom, but a lot of responsibility
as well... :)

- [reset-session-secret](#reset-session-secret)
- [session-cookie-name](#session-cookie-name)
- [session-cookie-value](#session-cookie-value)
- [session-created](#session-created)
- [next-session-id](#next-session-id)
- [session-db](#session-db)
- [session-db-lock](#session-db-lock)
- [session-verify](#session-verify)


### <span id="acceptor-behaviour">CUSTOMIZING ACCEPTOR BEHAVIOUR</span>

If you want to modify what acceptors do, you should subclass
`ACCEPTOR` (or `SSL-ACCEPTOR`) and specialize the
generic functions that constitute their behaviour (see example below).
The life of an acceptor looks like this: It is started with the function
`START` which immediately calls `START-LISTENING`
and then applies the function `EXECUTE-ACCEPTOR` to its
[taskmaster](#taskmasters). This function will eventually call
`ACCEPT-CONNECTIONS` which is responsible for setting things
up to wait for clients to connect. For each incoming connection which
comes in, `HANDLE-INCOMING-CONNECTION` is applied to the
taskmaster which will either call `PROCESS-CONNECTION`
directly, or will create a thread to call it.
`PROCESS-CONNECTION` calls
`INITIALIZE-CONNECTION-STREAM` before it does anything else,
then it selects and calls a function which handles the
[request](#requests), and finally it sends the [reply](#replies) to the
client before it calls `RESET-CONNECTION-STREAM`. If the
connection is persistent, this procedure is repeated (except for the
intialization step) in a loop until the connection is closed. The
acceptor is stopped with `STOP`.

If you just want to use the standard acceptors that come with
Hunchentoot, you don't need to know anything about the functions listed
in this section.

- [start-listening](#start-listening)
- [accept-connections](#accept-connections)
- [acceptor-log-access](#acceptor-log-access)
- [acceptor-log-message](#acceptor-log-message)
- [acceptor-status-message](#acceptor-status-message)
- [detach-socket](#detach-socket)
- [initialize-connection-stream](#initialize-connection-stream)
- [process-connection](#process-connection)
- [reset-connection-stream](#reset-connection-stream)

#### An example of how to subclass ACCEPTOR 

This example shows how to subclass `ACCEPTOR` in order to
provide Hunchentoot with basic virtual host support. It assumes
Hunchentoot is sitting behind an Internet-facing reverse-proxy web
server that maps the host (or domain) part of incoming HTTP requests to
unique localhost ports.

```lisp
    (ql:quickload '("hunchentoot" "drakma"))
```

```lisp
    ;;; Subclass ACCEPTOR
    (defclass vhost (hunchentoot:acceptor)
      ;; slots
      ((dispatch-table
        :initform '()
        :accessor dispatch-table
        :documentation "List of dispatch functions"))
      ;; options
      (:default-initargs                    ; default-initargs must be used
       :address "127.0.0.1"))               ; because ACCEPTOR uses it

    ;;; Specialise ACCEPTOR-DISPATCH-REQUEST for VHOSTs
    (defmethod hunchentoot:acceptor-dispatch-request ((vhost vhost) request)
      ;; try REQUEST on each dispatcher in turn
      (mapc (lambda (dispatcher)
          (let ((handler (funcall dispatcher request)))
            (when handler               ; Handler found. FUNCALL it and return result
              (return-from hunchentoot:acceptor-dispatch-request (funcall handler)))))
        (dispatch-table vhost))
      (call-next-method))

    ;;; ======================================================================
    ;;; Now all we need to do is test it

    ;;; Instantiate VHOSTs
    (defvar vhost1 (make-instance 'vhost :port 50001))
    (defvar vhost2 (make-instance 'vhost :port 50002))

    ;;; Populate each dispatch table
    (push
     (hunchentoot:create-prefix-dispatcher "/foo" 'foo1)
     (dispatch-table vhost1))
    (push
     (hunchentoot:create-prefix-dispatcher "/foo" 'foo2)
     (dispatch-table vhost2))

    ;;; Define handlers
    (defun foo1 () "Hello")
    (defun foo2 () "Goodbye")

    ;;; Start VHOSTs
    (hunchentoot:start vhost1)
    (hunchentoot:start vhost2)

    ;;; Make some requests
    (drakma:http-request "http://127.0.0.1:50001/foo")
    ;;; =|
    ;;; 127.0.0.1 - [2012-06-08 14:30:39] "GET /foo HTTP/1.1" 200 5 "-" "Drakma/1.2.6 (SBCL 1.0.56; Linux; 2.6.32-5-686; http://weitz.de/drakma/)"
    ;;; =>
    ;;; "Hello"
    ;;; 200
    ;;; ((:CONTENT-LENGTH . "5") (:DATE . "Fri, 08 Jun 2012 14:30:39 GMT")
    ;;;  (:SERVER . "Hunchentoot 1.2.3") (:CONNECTION . "Close")
    ;;;  (:CONTENT-TYPE . "text/html; charset=utf-8"))
    ;;; #<PURI:URI http://127.0.0.1:50001/foo>
    ;;; #<FLEXI-STREAMS:FLEXI-IO-STREAM {CA90059}>
    ;;; T
    ;;; "OK"
    (drakma:http-request "http://127.0.0.1:50002/foo")
    ;;; =|
    ;;; 127.0.0.1 - [2012-06-08 14:30:47] "GET /foo HTTP/1.1" 200 7 "-" "Drakma/1.2.6 (SBCL 1.0.56; Linux; 2.6.32-5-686; http://weitz.de/drakma/)"
    ;;; =>
    ;;; "Goodbye"
    ;;; 200
    ;;; ((:CONTENT-LENGTH . "7") (:DATE . "Fri, 08 Jun 2012 14:30:47 GMT")
    ;;;  (:SERVER . "Hunchentoot 1.2.3") (:CONNECTION . "Close")
    ;;;  (:CONTENT-TYPE . "text/html; charset=utf-8"))
    ;;; #<PURI:URI http://127.0.0.1:50002/foo>
    ;;; #<FLEXI-STREAMS:FLEXI-IO-STREAM {CAE8059}>
    ;;; T
    ;;; "OK"
```

How to make each VHOST write to separate access log streams (or files)
is left as an exercise to the reader.

### TASKMASTERS

As a "normal" Hunchentoot user, you can completely ignore taskmasters
and skip this section. But if you're still reading, here are the dirty
details: Each [acceptor](#acceptors) has a taskmaster associated with it
at creation time. It is the taskmaster's job to distribute the work of
accepting and handling incoming connections. The acceptor calls the
taskmaster if appropriate and the taskmaster calls back into the
acceptor. This is done using the generic functions described in this and
the [previous](#acceptor-behaviour) section. Hunchentoot comes with two
standard taskmaster implementations - one (which is the default used on
multi-threaded Lisps) which starts a new thread for each incoming
connection and one which handles all requests sequentially. It should
for example be relatively straightforward to create a taskmaster which
allocates threads from a fixed pool instead of creating a new one for
each connection.

You can control the resources consumed by a threaded taskmaster via two
initargs. `:max-thread-count` lets you set the maximum number of request
threads that can be processes simultaneously. If this is `nil`, the is
no thread limit imposed. `:max-accept-count` lets you set the maximum
number of requests that can be outstanding (i.e. being processed or
queued for processing). If `:max-thread-count` is supplied and
`:max-accept-count` is `NIL`, then a
`+HTTP-SERVICE-UNAVAILABLE+` error will be generated if there
are more than the max-thread-count threads processing requests. If both
`:max-thread-count` and `:max-accept-count` are supplied, then
max-thread-count must be less than max-accept-count; if more than
max-thread-count requests are being processed, then requests up to
max-accept-count will be queued until a thread becomes available. If
more than max-accept-count requests are outstanding, then a
`+HTTP-SERVICE-UNAVAILABLE+` error will be generated. In a
load-balanced environment with multiple Hunchentoot servers, it's
reasonable to provide `:max-thread-count` but leave `:max-accept-count`
null. This will immediately result in
`+HTTP-SERVICE-UNAVAILABLE+` when one server is out of
resources, so the load balancer can try to find another server. In an
environment with a single Hunchentoot server, it's reasonable to provide
both `:max-thread-count` and a somewhat larger value for
`:max-accept-count`. This will cause a server that's almost out of
resources to wait a bit; if the server is completely out of resources,
then the reply will be `+HTTP-SERVICE-UNAVAILABLE+`. The
default for these values is 100 and 120, respectively.

If you want to implement your own taskmasters, you should subclass
[TASKMASTER](#class-taskmaster) or one of its subclasses,
[SINGLE-THREADED-TASKMASTER](#single-threaded-taskmaster) or
[ONE-THREAD-PER-CONNECTION-TASKMASTER](#one-thread-per-connection-taskmaster), and specialize the
generic functions in this section.

- [execute-acceptor](#execute-acceptor)
- [handle-incoming-connection](#handle-incoming-connection)
- [start-thread](#start-thread)
- [create-request-handler-thread](#create-request-handler-thread)
- [shutdown](#shutdown)


Decodes a URL-encoded string which is assumed to be encoded using the
external format EXTERNAL-FORMAT, i.e. this is the inverse of
URL-ENCODE. It is assumed that you'll rarely need this function, if
ever. But just in case - here it is. The default for EXTERNAL-FORMAT is
the value of \*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT\*.


## SOME MORE TECHNICAL DETAILS

Hunchentoot will only work with Lisps where the [character
codes](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character_code)
of all [Latin-1](http://en.wikipedia.org/wiki/ISO/IEC_8859-1) characters
coincide with their Unicode [code
points](http://en.wikipedia.org/wiki/Code_point) (which is the case for
all current implementations I know).

Source code can be downloaded from <https://github.com/edicl/hunchentoot/archive/v1.2.38.tar.gz>.

The current development version of Hunchentoot can be found at
<https://github.com/edicl/hunchentoot>. If you want to send patches,
please fork the github repository and send pull requests.


### Without cl+ssl

You can compile Hunchentoot without
SSL support - and thus without the need to have CL+SSL - if you add
`:HUNCHENTOOT-NO-SSL` to
[`*FEATURES*`](http://www.lispworks.com/documentation/HyperSpec/Body/v_featur.htm)
*before* you compile it.

### clbuild 

Hunchentoot and its dependencies can also be installed with
[clbuild](http://common-lisp.net/project/clbuild/). 

### Gentoo Linux

There's also a port
for [Gentoo Linux](http://www.gentoo.org/proj/en/lisp/common-lisp/index.xml) thanks
to Matthew Kennedy.


### Running Hunchentoot on port 80

Hunchentoot does not come with code to help with running it on a
privileged port (i.e. port 80 or 443) on Unix-like operating systems.
Modern Unix-like systems have specific, non-portable ways to allow
non-root users to listen to privileged ports, so including such
functionality in Hunchentoot was considered unnecessary. Please refer to
online resources for help. At the time of this writing, the YAWS
documentation has a [comprehensive
writeup](http://yaws.hyber.org/privbind.yaws) on the topic.

### Hunchentoot behind a proxy

If you're feeling unsecure about exposing Hunchentoot to the wild, wild
Internet or if your Lisp web application is part of a larger website,
you can hide it behind a [proxy
server](http://en.wikipedia.org/wiki/Proxy_server). One approach that I
have used several times is to employ Apache's
[mod\_proxy](http://httpd.apache.org/docs/current/mod/mod_proxy.html)
module with a configuration that looks like this:

    ProxyPass /hunchentoot http://127.0.0.1:3000/hunchentoot
    ProxyPassReverse /hunchentoot http://127.0.0.1:3000/hunchentoot

This will tunnel all requests where the URI path begins with
`"/hunchentoot"` to a (Hunchentoot) server listening on port 3000 on the
same machine.

Of course, there are [several
other](http://www.red-bean.com/pipermail/lispweb/2006-October/001342.html)
(more lightweight) web proxies that you could use instead of Apache.

## SUPPORT

The development version of Hunchentoot can be found [on
github](https://github.com/edicl/hunchentoot). Please use the github
issue tracking system to submit bug reports. Patches are welcome, please
use [GitHub pull requests](https://github.com/edicl/hunchentoot/pulls).
If you want to make a change, please [read this
first](http://weitz.de/patches.html).

