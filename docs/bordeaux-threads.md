# bordeaux-threads - Threading

Version: 0.8.7
<br/>
Repository: [sionescu/bordeaux-threads - Github](https://github.com/sionescu/bordeaux-threads/)

*This page was possible due to the [official documentation](https://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation), albeit its a bit outdated.*

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/digikar99/common-lisp.readthedocs/issues).*

***


BORDEAUX-THREADS is a proposed standard for a minimal
  MP/threading interface. It is similar to the CLIM-SYS threading and
  lock support, but for the following broad differences:

  1. Some behaviours are defined in additional detail: attention has
     been given to special variable interaction, whether and when
     cleanup forms are run. Some behaviours are defined in less
     detail: an implementation that does not support multiple
     threads is not required to use a new list (nil) for a lock, for
     example.

  2. Many functions which would be difficult, dangerous or inefficient
     to provide on some implementations have been removed. Chiefly
     these are functions such as thread-wait which expect for
     efficiency that the thread scheduler is written in Lisp and
     'hookable', which can't sensibly be done if the scheduler is
     external to the Lisp image, or the system has more than one CPU.

  3. Unbalanced ACQUIRE-LOCK and RELEASE-LOCK functions have been
     added.

  4. Posix-style condition variables have been added, as it's not
     otherwise possible to implement them correctly using the other
     operations that are specified.

  Threads may be implemented using whatever applicable techniques are
  provided by the operating system: user-space scheduling,
  kernel-based LWPs or anything else that does the job.

  Some parts of this specification can also be implemented in a Lisp
  that does not support multiple threads. Thread creation and some
  thread inspection operations will not work, but the locking
  functions are still present (though they may do nothing) so that
  thread-safe code can be compiled on both multithread and
  single-thread implementations without need of conditionals.

  To avoid conflict with existing MP/threading interfaces in
  implementations, these symbols live in the BORDEAUX-THREADS package.
  Implementations and/or users may also make them visible or exported
  in other more traditionally named packages.

### \*default-special-bindings\*

```lisp
Variable
```

This variable holds an alist associating special variable symbols
  to forms to evaluate. Special variables named in this list will
  be locally bound in the new thread before it begins executing user code.

  This variable may be rebound around calls to [make-thread](#make-thread) to
  add/alter default bindings. The effect of mutating this list is
  undefined, but earlier forms take precedence over later forms for
  the same symbol, so defaults may be overridden by consing to the
  head of the list.

### \*standard-io-bindings\*

```lisp
Variable
```

Standard bindings of printer/reader control variables as per CL:WITH-STANDARD-IO-SYNTAX.

### \*supports-threads-p\*

```lisp
Variable
```

This should be set to T if the running instance has thread support.

### acquire-lock

```lisp
Function: (acquire-lock lock &optional (wait-p t))
```
Acquire the lock `lock` for the calling thread.
  `wait-p` governs what happens if the lock is not available: if `wait-p`
  is true, the calling thread will wait until the lock is available
  and then acquire it; if `wait-p` is NIL, `acquire-lock` will return
  immediately. `acquire-lock` returns true if the lock was acquired and
  NIL otherwise.

  This specification does not define what happens if a thread
  attempts to acquire a lock that it already holds. For applications
  that require locks to be safe when acquired recursively, see instead
  [make-recursive-lock](#make-recursive-lock) and friends.

### acquire-recursive-lock

```lisp
Function: (acquire-recursive-lock lock)
```
As for [acquire-lock](#acquire-lock), but for recursive locks.

### all-threads

```lisp
Function: (all-threads)
```
Returns a sequence of all of the threads. This may not
  be freshly-allocated, so the caller should not modify it.

### condition-notify

```lisp
Function: (condition-notify condition-variable)
```
Notify at least one of the threads waiting for
  `condition-variable`. It is implementation-dependent whether one or
  more than one (and possibly all) threads are woken, but if the
  implementation is capable of waking only a single thread (not all
  are) this is probably preferable for efficiency reasons. The order
  of wakeup is unspecified and does not necessarily relate to the
  order that the threads went to sleep in.

  `condition-notify` has no useful return value. In an implementation
  that does not support multiple threads, it has no effect.

### condition-wait

```lisp
Function: (condition-wait condition-variable lock &key timeout)
```
Atomically release `lock` and enqueue the calling
  thread waiting for `condition-variable`. The thread will resume when
  another thread has notified it using [condition-notify](#condition-notify); it may also
  resume if interrupted by some external event or in other
  implementation-dependent circumstances: the caller must always test
  on waking that there is threading to be done, instead of assuming
  that it can go ahead.

  It is an error to call function this unless from the thread that
  holds `lock`.

  If `timeout` is nil or not provided, the system always reacquires `lock`
  before returning to the caller. In this case T is returned.

  If `timeout` is non-nil, the call will return after at most `timeout`
  seconds (approximately), whether or not a notification has occurred.
  Either NIL or T will be returned. A return of NIL indicates that the
  lock is no longer held and that the timeout has expired. A return of
  T indicates that the lock is held, in which case the timeout may or
  may not have expired.

  **NOTE**: The behavior of `condition-wait` with `timeout` diverges from
  the POSIX function pthread_cond_timedwait. The former may return
  without the lock being held while the latter always returns with the
  lock held.

  In an implementation that does not support multiple threads, this
  function signals an error.

### current-thread

```lisp
Function: (current-thread)
```
Returns the thread object for the calling
  thread. This is the same kind of object as would be returned by
  [make-thread](#make-thread).

### destroy-thread

```lisp
Function: (destroy-thread thread)
```
Terminates the thread `thread`, which is an object
  as returned by [make-thread](#make-thread). This should be used with caution: it is
  implementation-defined whether the thread runs cleanup forms or
  releases its locks first.

  Destroying the calling thread is an error.

### interrupt-thread

```lisp
Function: (interrupt-thread thread function &rest args)
```
Interrupt `thread` and cause it to evaluate `function`
  before continuing with the interrupted path of execution. This may
  not be a good idea if `thread` is holding locks or doing anything
  important. On systems that do not support multiple threads, this
  function signals an error.

### join-thread

```lisp
Function: (join-thread thread)
```
Wait until `thread` terminates. If `thread` has already terminated,
  return immediately. The return values of the thread function are
  returned.

### lock

### lock-p

```lisp
Function: (lock-p object)
```
Returns T if `object` is a lock; returns NIL otherwise.

### make-condition-variable

```lisp
Function: (make-condition-variable &key name)
```
Returns a new condition-variable object for use
  with [condition-wait](#condition-wait) and [condition-notify](#condition-notify).

### make-lock

```lisp
Function: (make-lock &optional name)
```
Creates a lock (a mutex) whose name is `name`. If the system does not
  support multiple threads this will still return some object, but it
  may not be used for very much.

### make-recursive-lock

```lisp
Function: (make-recursive-lock &optional name)
```
Create and return a recursive lock whose name is `name`. A recursive
  lock differs from an ordinary lock in that a thread that already
  holds the recursive lock can acquire it again without blocking. The
  thread must then release the lock twice before it becomes available
  for another thread.

### make-semaphore

```lisp
Function: (make-semaphore &key name (count 0))
```
Create a semaphore with the supplied `name` and initial counter value `count`.

### make-thread

```lisp
Function: (make-thread function &key name (initial-bindings
                                           *default-special-bindings*))
```
Creates and returns a thread named `name`, which will call the
  function `function` with no arguments: when `function` returns, the
  thread terminates. `name` defaults to "Anonymous thread" if unsupplied.

  On systems that do not support multi-threading, `make-thread` will
  signal an error.

  The interaction between threads and dynamic variables is in some
  cases complex, and depends on whether the variable has only a global
  binding (as established by e.g. DEFVAR/DEFPARAMETER/top-level SETQ)
  or has been bound locally (e.g. with LET or LET*) in the calling
  thread.

  - Global bindings are shared between threads: the initial value of a
    global variable in the new thread will be the same as in the
    parent, and an assignment to such a variable in any thread will be
    visible to all threads in which the global binding is visible.

  - Local bindings, such as the ones introduced by `initial-bindings`,
    are local to the thread they are introduced in, except that

  - Local bindings in the the caller of `make-thread` may or may not be
    shared with the new thread that it creates: this is
    implementation-defined. Portable code should not depend on
    particular behaviour in this case, nor should it assign to such
    variables without first rebinding them in the new thread.

### recursive-lock

### recursive-lock-p

```lisp
Function: (recursive-lock-p object)
```
Returns T if `object` is a recursive lock; returns NIL otherwise.

### release-lock

```lisp
Function: (release-lock lock)
```
Release `lock`. It is an error to call this unless
  the lock has previously been acquired (and not released) by the same
  thread. If other threads are waiting for the lock, the
  [acquire-lock](#acquire-lock) call in one of them will now be able to continue.

  This function has no interesting return value.

### release-recursive-lock

```lisp
Function: (release-recursive-lock lock)
```
Release the recursive `lock`. The lock will only
  become free after as many Release operations as there have been
  Acquire operations. See [release-lock](#release-lock) for other information.

### semaphore

### semaphore-p

```lisp
Function: (semaphore-p object)
```
Returns T if `object` is a semaphore; returns NIL otherwise.

### signal-semaphore

```lisp
Function: (signal-semaphore semaphore &key (count 1))
```
Increment `semaphore` by `count`. If there are threads waiting on this
semaphore, then `count` of them are woken up.

### start-multiprocessing

```lisp
Function: (start-multiprocessing)
```
If the host implementation uses user-level threads, start the
scheduler and multiprocessing, otherwise do nothing.
It is safe to call repeatedly.

### thread

### thread-alive-p

```lisp
Function: (thread-alive-p thread)
```
Returns true if `thread` is alive, that is, if
  [destroy-thread](#destroy-thread) has not been called on it.

### thread-name

```lisp
Function: (thread-name thread)
```
Returns the name of the thread, as supplied to [make-thread](#make-thread).

### thread-yield

```lisp
Function: (thread-yield)
```
Allows other threads to run. It may be necessary or desirable to
  call this periodically in some implementations; others may schedule
  threads automatically. On systems that do not support
  multi-threading, this does nothing.

### threadp

```lisp
Function: (threadp object)
```
Returns true if object is a thread, otherwise NIL.

### timeout

### wait-on-semaphore

```lisp
Function: (wait-on-semaphore semaphore &key timeout)
```
Decrement the count of `semaphore` by 1 if the count would not be negative.

Else blocks until the semaphore can be decremented. Returns generalized boolean
T on success.

If `timeout` is given, it is the maximum number of seconds to wait. If the count
cannot be decremented in that time, returns NIL without decrementing the count.

### with-lock-held

```lisp
Macro: (with-lock-held (place) &body body)
```
Evaluates `body` with the lock named by `place`, the value of which
  is a lock created by [make-lock](#make-lock). Before the forms in `body` are
  evaluated, the lock is acquired as if by using [acquire-lock](#acquire-lock). After the
  forms in `body` have been evaluated, or if a non-local control transfer
  is caused (e.g. by THROW or SIGNAL), the lock is released as if by
  [release-lock](#release-lock).

  Note that if the debugger is entered, it is unspecified whether the
  lock is released at debugger entry or at debugger exit when execution
  is restarted.

### with-recursive-lock-held

```lisp
Macro: (with-recursive-lock-held (place) &body body)
```
Evaluates `body` with the recursive lock named by `place`, which is a
reference to a recursive lock created by [make-recursive-lock](#make-recursive-lock). See
[with-lock-held](#with-lock-held) etc etc

### with-timeout

```lisp
Macro: (with-timeout (timeout) &body body)
```
