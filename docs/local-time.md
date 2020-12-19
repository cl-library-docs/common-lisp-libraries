# local-time - Date and Time Manipulation

Version: 1.0.6
<br/>
Licence: 3-clause BSD
<br/>
Repository: [dlowe-net/local-time](https://github.com/dlowe-net/local-time)
<br>
See also: [awesome-cl#date-and-time](https://github.com/CodyReichert/awesome-cl#date-and-time)

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/cl-library-docs/common-lisp-libraries/issues).*

*Great thanks to the excellent [official documentation](https://common-lisp.net/project/local-time/manual.html).*

`local-time` library is a Common Lisp library for the manipulation
of dates, times and intervals. It was originally based almost entirely
upon Erik Naggum's paper [The Long Painful History of Time](http://naggum.no/lugm-time.html). Many of the core concepts originated from this paper,
such as the seperation of days and seconds, the choice of 2000-03-01 as
the standard epoch, and the timestring format.


## GETTING STARTED

### timestamps and timezones

`local-time` deals with time in the form of [timestamp](#timestamp) and [timezone](#timezone).

The following constructs deal with timezones:

- [\*default-timezone\*](#default-timezone)
- [+utc-zone+](#utc-zone)
- [define-timezone](#define-timezone)
- [find-timezone-by-location-name](#find-timezone-by-location-name)
- [reread-timezone-repository](#reread-timezone-repository)


**Examples**

```lisp
CL-USER> (add-package-local-nickname :lt :local-time)
#<PACKAGE "COMMON-LISP-USER">

CL-USER> (lt:now)
@2020-12-16T13:44:22.493573Z

CL-USER> lt:+asctime-format+
(:SHORT-WEEKDAY #\  :SHORT-MONTH #\  (:DAY 2 #\ ) #\  (:HOUR 2) #\: (:MIN 2)
 #\: (:SEC 2) #\  (:YEAR 4))

CL-USER> (lt:format-timestring nil (lt:now)
                               :format lt:+asctime-format+)
"Wed Dec 16 13:45:24 2020"

CL-USER> (lt:reread-timezone-repository)
NIL

CL-USER> lt::*default-timezone-repository-path*
#P"/home/user/quicklisp/local-projects/local-time/zoneinfo/"

CL-USER> (lt:find-timezone-by-location-name "US/Eastern")
#<LT::TIMEZONE LMT EDT EST EWT EPT>
T

CL-USER> (lt:format-timestring nil (lt:now)
                               :format lt:+asctime-format+
                               :timezone *)
"Wed Dec 16 09:00:57 2020"
```

See [the cookbook](https://lispcookbook.github.io/cl-cookbook/dates_and_times.html) for more examples.

### Dealing with timestamps

**Conversion: to and fro**

- [now](#now)
- [today](#today)
<br/>
<br/>
- [days-in-month](#days-in-month)
<br/>
<br/>
- [universal-to-timestamp](#universal-to-timestamp)
- [unix-to-timestamp](#unix-to-timestamp)
- [timestamp-to-universal](#timestamp-to-universal)
- [timestamp-to-unix](#timestamp-to-unix)
<br/>
<br/>
- [make-timestamp](#make-timestamp)
- [clone-timestamp](#clone-timestamp)
<br/>
<br/>
- [encode-timestamp](#encode-timestamp)
- [decode-timestamp](#decode-timestamp)
- [with-decoded-timestamp](#with-decoded-timestamp)

**Querying and Manipulation**

- [day-of](#day-of)
- [sec-of](#sec-of)
- [nsec-of](#nsec-of)
<br/>
<br/>
- [timestamp<](#timestamp<)
- [timestamp<=](#timestamp<=)
- [timestamp>](#timestamp>)
- [timestamp>=](#timestamp>=)
- [timestamp=](#timestamp=)
- [timestamp/=](#timestamp/=)
- [timestamp-minimum](#timestamp-minimum)
- [timestamp-maximum](#timestamp-maximum)
- [timestamp-whole-year-difference](#timestamp-whole-year-difference)
<br/>
<br/>
- [timestamp+](#timestamp+)
- [timestamp-](#timestamp-)
- [timestamp-maximize-part](#timestamp-maximize-part)
- [timestamp-minimize-part](#timestamp-minimize-part)
<br/>
<br/>
- [adjust-timestamp](#adjust-timestamp)
- [adjust-timestamp!](#adjust-timestamp!)
<br/>
<br/>
- [timestamp-subtimezone](#timestamp-subtimezone)
- [timestamp-day-of-week](#timestamp-day-of-week)
- [timestamp-millennium](#timestamp-millennium)
- [timestamp-century](#timestamp-century)
- [timestamp-decade](#timestamp-decade)
- [timestamp-year](#timestamp-year)
- [timestamp-month](#timestamp-month)
- [timestamp-day](#timestamp-day)
- [timestamp-hour](#timestamp-hour)
- [timestamp-minute](#timestamp-minute)
- [timestamp-second](#timestamp-second)
- [timestamp-millisecond](#timestamp-millisecond)
- [tiemstamp-microsecond](#tiemstamp-microsecond)

**Parsing and Formatting**

- [+iso-8601-format+](#iso-8601-format)
- [+asctime-format+](#asctime-format)
- [+rfc-1123-format+](#rfc-1123-format)
- [+iso-week-date-format+](#iso-week-date-format)
- [parse-timestring](#parse-timestring)
- [format-timestring](#format-timestring)
- [format-rfc3339-timestring](#format-rfc3339-timestring)

**Reader Macros**

- [enable-read-macros](#enable-read-macros)

### Clocks

The [\*clock\*](#clock) special variable and the generic functions [clock-now](#clock-now) and [clock-today](#clock-today) are exposed so that applications may re-define the current time or date as required. This can be used for testing or to support alternate clocks.

The currently supported values are:

- `t` - Use the standard system clock with no adjustments
- `leap-second-adjusted` - The system clock, adjusted for leap seconds using the information in [\*default-timezone\*](#default-timezone).


### Non-Gregorian Calendars

Support for julian calendars is provided by the following functions:

- [astronomical-julian-date](#astronomical-modified-julian-date)
- [astronomical-modified-julian-date](#astronomical-modified-julian-date)

### A note on Portability

This implementation assumes that time zone information is stored in the
tzfile format. The default timezone is loaded from /etc/localtime. On
non-POSIX systems, this will certainly give different results than the
system time handling.

local-time currently supports subsecond precision clocks with allegro,
cmucl, sbcl, abcl, and non-Windows ccl. All others will be able to
retrieve the time with second precision using `get-universal-time`. You
may add support for your own implementation by implementing the clock
generic protocol documented here.

## API REFERENCE

### \*clock\*

```lisp
Variable
Default Value: T
```

Use this special variable if you need to define your own idea of the current time.

The value of this variable should have the methods [clock-now](#clock-now), and
[clock-today](#clock-today). The currently supported values in `local-time` are:

-  `t` - use the standard clock
-  [leap-second-adjusted](#leap-second-adjusted) - use a clock which adjusts for leap seconds using the information in [\*default-timezone\*](#default-timezone).

TODO: Add / Point to a concrete example of dealing with clocks.

### \*default-timezone\*

```lisp
Variable
Default Value: #<LOCAL-TIME::TIMEZONE UTC>
```

This variable contains the timezone that will be used by default if none
is specified. It is loaded from `/etc/localtime` when the library is loaded.
If `/etc/localtime` is not present, it will default to UTC.

### +asctime-format+

```lisp
Variable
Default Value: (:SHORT-WEEKDAY #\  :SHORT-MONTH #\  (:DAY 2 #\ ) #\  (:HOUR 2)
                #\: (:MIN 2) #\: (:SEC 2) #\  (:YEAR 4))
```

This constant is bound to a format mirroring the output of the POSIX
asctime() function. An output with this format will look like this:
`Sat Mar  1 19:42:34 2008`.

### +day-names+

```lisp
Variable
Default Value: #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
                 "Saturday")
```

### +days-per-week+

```lisp
Constant: 7
```

### +gmt-zone+

```lisp
Variable
Default Value: #<LOCAL-TIME::TIMEZONE GMT>
```

### +hours-per-day+

```lisp
Constant: 24
```

### +iso-8601-date-format+

```lisp
Variable
Default Value: ((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2))
```

See [+iso-8601-format+](#iso-8601-format).

### +iso-8601-format+

```lisp
Variable
Default Value: ((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\T (:HOUR 2) #\:
                (:MIN 2) #\: (:SEC 2) #\. (:USEC 6) :GMT-OFFSET-OR-Z)
```

This constant is bound to a description of the ISO 8601 format. An output
with this format will look like this: `2008-03-01T19:42:34.608506+01:00`.
This is the default format for the [format-timestring](#format-timestring) function.

### +iso-8601-time-format+

```lisp
Variable
Default Value: ((:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) #\. (:USEC 6))
```

See [+iso-8601-format+](#iso-8601-format).

### +iso-week-date-format+

```lisp
Variable
Default Value: ((:ISO-WEEK-YEAR 4) #\- #\W (:ISO-WEEK-NUMBER 2) #\-
                (:ISO-WEEK-DAY 1))
```

This constant is bound to a description of the ISO 8601 Week Date format.
An output with this format will look like this: `2009-W53-5`.

### +minutes-per-day+

```lisp
Constant: 1440
```

### +minutes-per-hour+

```lisp
Constant: 60
```

### +month-names+

```lisp
Variable
Default Value: #("" "January" "February" "March" "April" "May" "June" "July"
                 "August" "September" "October" "November" "December")
```

### +months-per-year+

```lisp
Constant: 12
```

### +rfc-1123-format+

```lisp
Variable
Default Value: (:SHORT-WEEKDAY ", " (:DAY 2) #\  :SHORT-MONTH #\  (:YEAR 4) #\
                (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) #\  :GMT-OFFSET-HHMM)
```

This constant is bound to a description of the format defined in RFC 1123
for Internet timestamps. An output with this format will look like this:
`Sat, 01 Mar 2008 19:42:34 -0500`. See the [RFC 1123](https://tools.ietf.org/html/rfc1123)
for the details about the possible values of the timezone field.

### +rfc3339-format+

```lisp
Variable
Default Value: ((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\T (:HOUR 2) #\:
                (:MIN 2) #\: (:SEC 2) #\. (:USEC 6) :GMT-OFFSET-OR-Z)
```

### +rfc3339-format/date-only+

```lisp
Variable
Default Value: ((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2))
```

### +seconds-per-day+

```lisp
Constant: 86400
```

### +seconds-per-hour+

```lisp
Constant: 3600
```

### +seconds-per-minute+

```lisp
Constant: 60
```

### +short-day-names+

```lisp
Variable
Default Value: #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
```

### +short-month-names+

```lisp
Variable
Default Value: #("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct"
                 "Nov" "Dec")
```

### +utc-zone+

```lisp
Variable
Default Value: #<LOCAL-TIME::TIMEZONE UTC>
```

A timezone corresponding to UTC.

### adjust-timestamp

```lisp
Macro: (adjust-timestamp timestamp &body changes)
```

Alters various parts of [timestamp](#timestamp), given a list of
changes. The changes are in the format `(offset part value)` and
`(set part value)`.

``` {.lisp}
 ;; Return a new timestamp value that points to the previous Monday
 (adjust-timestamp (today) (offset :day-of-week :monday))

 ;; Return a new timestamp value that points three days ahead from now
 (adjust-timestamp (today) (offset :day 3))
```

Keep in mind that adjust-timestamp is not a mere setter for fields
but instead it handles overflows and timezone conversions as
expected. Also note that it's possible to specify multiple commands.

The list of possible places to manipulate are: `:nsec` `:sec`
`:sec-of-day` `:minute` `:hour` `:day` `:day-of-month` `:month`
`:year`.


### adjust-timestamp!

```lisp
Macro: (adjust-timestamp! timestamp &body changes)
```

Just like [adjust-timestamp](#adjust-timestamp), but instead of returning a freshly constructed
value, it alters the provided [timestamp](#timestamp) value (and returns it).

### all-timezones-matching-subzone

```lisp
Function: (all-timezones-matching-subzone abbreviated-name)
```

Returns list of lists of timezone, matched subzone and last transition time
   for timezones that have subzone matching specified abbreviated-name. Includes both active and historical timezones.

### astronomical-julian-date

```lisp
Function: (astronomical-julian-date timestamp)
```

Returns the astronomical julian date referred to by the timestamp.

### astronomical-modified-julian-date

No documentation found for `astronomical-modified-julian-date`

### clock-now

```lisp
Generic Function: (clock-now clock)
```

Returns a timestamp for the current time given a clock.
Specialize this generic function to re-define the present moment

### clock-today

```lisp
Generic Function: (clock-today clock)
```

Returns a timestamp for the current date given a
  clock.  The date is encoded by convention as a timestamp with the
  time set to 00:00:00UTC.

Specialize this generic function to re-define the present day.

### clone-timestamp

```lisp
Function: (clone-timestamp timestamp)
```

Returns a copy of [timestamp](#timestamp) that is `timestamp=` to it.

### date

```lisp
Type
```

A [timestamp](#timestamp) referring to a UTC timezone. The `sec` slot must be the first
second of a day; in other words, the time elements of the `timestamp` value must
have their least possible values 0.


### day-of

```lisp
Generic Function: (day-of object)
```

Returns the day component of [timestamp](#timestamp). Although
Naggum's paper specifies that the day should be a signed fixnum, it
is left unbounded for flexibility reasons.


### days-in-month

```lisp
Function: (days-in-month month year)
```

Returns the number of days in the given month of the specified year.

### decode-timestamp

```lisp
Function: (decode-timestamp timestamp &key (timezone *default-timezone*) offset)
```

Returns the decoded time as multiple values:

    (values ns ss mm hh day month year day-of-week daylight-saving-time-p timezone-offset timezone-abbreviation)

### define-timezone

```lisp
Macro: (define-timezone zone-name zone-file &key (load NIL))
```

Define [zone-name](#zone-name) (a symbol or a string) as a new timezone, lazy-loaded from zone-file (a pathname designator relative to the zoneinfo directory on this system.  If load is true, load immediately.

### enable-read-macros

```lisp
Function: (enable-read-macros)
```

Enables the local-time reader macros for literal timestamps and universal time.

### encode-timestamp

```lisp
Function: (encode-timestamp nsec sec minute hour day month year &key
           (timezone *default-timezone*) offset into)
```

Returns a new [timestamp](#timestamp) instance corresponding to the specified
time elements. offset is the number of seconds offset from UTC of the locale.
If offset is not specified, the offset will be guessed from the timezone.

If a [timestamp](#timestamp) is passed as the into argument, its value will be set and that
[timestamp](#timestamp) will be returned. Otherwise, a new [timestamp](#timestamp) is created.

### find-timezone-by-location-name

```lisp
Function: (find-timezone-by-location-name name)
```

Returns the timezone found at the location name (such as `US/Eastern`).
[reread-timezone-repository](#reread-timezone-repository) must be called before this function is used.

### format-rfc1123-timestring

```lisp
Function: (format-rfc1123-timestring destination timestamp &key
           (timezone *default-timezone*))
```

See [+rfc-1123-format+](#rfc-1123-format) and [format-timestring](#format-timestring).


### format-rfc3339-timestring

```lisp
Function: (format-rfc3339-timestring destination timestamp &key omit-date-part
           omit-time-part (omit-timezone-part omit-time-part) (use-zulu t)
           (timezone *default-timezone*))
```

Formats a timestring in the RFC 3339 format, a restricted form of the ISO-8601 timestring specification for Internet timestamps.

See [+rfc3339-format+](#rfc3339-format) and [format-timestring](#format-timestring).

### format-timestring

```lisp
Function: (format-timestring destination timestamp &key
           (format +iso-8601-format+) (timezone *default-timezone*))
```

Constructs a string representation of [timestamp](#timestamp) according
to format and returns it.
If `destination` is T, the string is written to *STANDARD-OUTPUT*.
If `destination` is a stream, the string is written to the stream.

`format` is a list containing one or more of strings, characters,
and keywords. Strings and characters are output literally,
while keywords are replaced by the values here:

    :YEAR              *year
    :MONTH             *numeric month
    :DAY               *day of month
    :HOUR              *hour
    :MIN               *minutes
    :SEC               *seconds
    :WEEKDAY           *numeric day of week starting from index 0, which means Sunday
    :MSEC              *milliseconds
    :USEC              *microseconds
    :NSEC              *nanoseconds
    :ISO-WEEK-YEAR     *year for ISO week date (can be different from regular calendar year)
    :ISO-WEEK-NUMBER   *ISO week number (i.e. 1 through 53)
    :ISO-WEEK-DAY      *ISO compatible weekday number (monday=1, sunday=7)
    :LONG-WEEKDAY      long form of weekday (e.g. Sunday, Monday)
    :SHORT-WEEKDAY     short form of weekday (e.g. Sun, Mon)
    :MINIMAL-WEEKDAY   minimal form of weekday (e.g. Su, Mo)
    :SHORT-YEAR        short form of year (last 2 digits, e.g. 41, 42 instead of 2041, 2042)
    :LONG-MONTH        long form of month (e.g. January, February)
    :SHORT-MONTH       short form of month (e.g. Jan, Feb)
    :HOUR12            *hour on a 12-hour clock
    :AMPM              am/pm marker in lowercase
    :GMT-OFFSET        the gmt-offset of the time, in +00:00 form
    :GMT-OFFSET-OR-Z   like :GMT-OFFSET, but is Z when UTC
    :GMT-OFFSET-HHMM   like :GMT-OFFSET, but in +0000 form
    :TIMEZONE          timezone abbrevation for the time

Elements marked by `*` can be placed in a list in the form

    (:keyword padding &optional (padchar #\0))

The string representation of the value will be padded with the padchar.

You can see examples in [+ISO-8601-FORMAT+](#iso-8601-format), [+ASCTIME-FORMAT+](#asctime-format), and [+RFC-1123-FORMAT+](#rfc-1123-format).

### invalid-timestring

```lisp
Condition
```


### leap-second-adjusted

No documentation found for `leap-second-adjusted`

### make-timestamp

```lisp
Macro: (make-timestamp &rest args)
```

Expands to an expression that creates an instance of a [timestamp](#timestamp)
    exactly as specified.

### modified-julian-date

```lisp
Function: (modified-julian-date timestamp)
```

Returns the modified julian date referred to by the timestamp.

### now

```lisp
Function: (now)
```

Produces a [timestamp](#timestamp) instance with the current time. Under sbcl, the new timestamp will be precise to the microsecond. Otherwise, the precision is limited to the second.

### nsec-of

```lisp
Generic Function: (nsec-of object)
```
Returns the `microseconds` component of the [timestamp](#timestamp). Valid values for
    the nanoseconds range from 0 to 999999999.



### parse-rfc3339-timestring

```lisp
Function: (parse-rfc3339-timestring timestring &key (fail-on-error t)
           (allow-missing-time-part NIL))
```

### parse-timestring

```lisp
Function: (parse-timestring timestring &key start end (fail-on-error t)
           (time-separator :) (date-separator -) (date-time-separator t)
           (fract-time-separators (quote (. ,))) (allow-missing-elements t)
           (allow-missing-date-part allow-missing-elements)
           (allow-missing-time-part allow-missing-elements)
           (allow-missing-timezone-part allow-missing-elements) (offset 0))
```

Parses a timestring and returns the corresponding `timestamp`.
Parsing begins at `start` and stops at the
`end` position. If there are invalid characters within
`timestring` and `fail-on-error` is `T`, then an
`invalid-timestring` error is signaled, otherwise `NIL` is returned.

If there is no timezone specified in `timestring` then
`offset` is used as the default timezone offset (in
seconds).

See `local-time::split-timestring` for details. Unspecified fields in the
timestring are initialized to their lowest possible value,
and timezone offset is 0 (UTC) unless explicitly specified
in the input string.

### reread-timezone-repository

```lisp
Function: (reread-timezone-repository &key
           (timezone-repository *default-timezone-repository-path*))
```

Walks the current repository, reading all tzinfo files updating indexes.
The default timezone repository is set to the zoneinfo/ directory of the local-time system.

### sec-of

```lisp
Generic Function: (sec-of object)
```

Returns the `seconds` component of the [timestamp](#timestamp). Valid values for the
    seconds range from 0 to 86399.


### time-of-day

```lisp
Type
```

A [timestamp](#timestamp) with `day` slot having the value 0.


### timestamp

```lisp
Class
```

`timestamp` values are basically instances of

```lisp
 (defclass timestamp ()
   ((day :type integer)
    (sec :type integer)
    (nsec :type (integer 0 999999999))))
```

These can represent either a [date](#date), a `daytime` or a
[time-of-day](#time-of-day) value, depending on the values of its slots.


### timestamp+

```lisp
Function: (timestamp+ time amount unit &optional (timezone *default-timezone*)
           offset)
```

Add the `amount` to the `time`
    using the specified `unit`.

- `unit` may be one of ( `:nsec` `:sec` `:minute` `:hour` `:day` `:month` `:year`).

The value of the parts of the timestamp of higher resolution than the
    `unit` will never be touched. If you want a precise number of seconds
    from a time, you should specify the offset in seconds.

### timestamp-

```lisp
Function: (timestamp- time amount unit &optional (timezone *default-timezone*)
           offset)
```

Subtract the `amount` from the `time`
    using the specified `unit`.

- `unit` may be one of ( `:nsec` `:sec` `:minute` `:hour` `:day` `:month` `:year`).

The value of the parts of the timestamp of higher resolution than the
    `unit` will never be touched. If you want a precise number of seconds
    from a time, you should specify the offset in seconds.

### timestamp-century

```lisp
Function: (timestamp-century timestamp &key (timezone *default-timezone*))
```

Returns the ordinal century upon which the timestamp falls. Ordinal time values start at 1, so the `(timestamp-century (now))` will return 21.

### timestamp-day

```lisp
Function: (timestamp-day timestamp &key (timezone *default-timezone*))
```

Returns the day of the month upon which the timestamp falls.

### timestamp-day-of-week

```lisp
Function: (timestamp-day-of-week timestamp &key (timezone *default-timezone*)
           offset)
```

This returns the index of the day of the week, starting at 0 which
    means Sunday.

**Note:** "Day of the week" is ambigous and locale dependent.

### timestamp-decade

```lisp
Function: (timestamp-decade timestamp &key (timezone *default-timezone*))
```

Returns the cardinal decade upon which the timestamp falls. Ordinal time values start at 1.

### timestamp-difference

```lisp
Function: (timestamp-difference time-a time-b)
```

Returns the difference between time-a and time-b in seconds

### timestamp-hour

```lisp
Function: (timestamp-hour timestamp &key (timezone *default-timezone*))
```

### timestamp-maximize-part

```lisp
Function: (timestamp-maximize-part timestamp part &key
           (timezone *default-timezone*) into)
```

Returns a timestamp with its parts maximized up to
    `part`. `part` can be any of (:nsec :sec :min
    :hour :day :month). If `into` is specified, it will be
    modified and returned, otherwise a new timestamp will be created.

### timestamp-maximum

```lisp
Function: (timestamp-maximum time &rest times)
```

Returns the latest timestamp

### timestamp-microsecond

```lisp
Function: (timestamp-microsecond timestamp)
```

### timestamp-millennium

```lisp
Function: (timestamp-millennium timestamp &key (timezone *default-timezone*))
```

Returns the ordinal millennium upon which the timestamp falls. These start from 1, so the `(timestamp-millennium (now))` will return 3.

### timestamp-millisecond

```lisp
Function: (timestamp-millisecond timestamp)
```

### timestamp-minimize-part

```lisp
Function: (timestamp-minimize-part timestamp part &key
           (timezone *default-timezone*) into)
```

Returns a timestamp with its parts minimized up to
    `part`. `part` can be any of (:nsec :sec :min
    :hour :day :month). If `into` is specified, it will be
    modified and returned, otherwise a new timestamp will be created.


### timestamp-minimum

```lisp
Function: (timestamp-minimum time &rest times)
```

Returns the earliest timestamp

### timestamp-minute

```lisp
Function: (timestamp-minute timestamp &key (timezone *default-timezone*))
```

### timestamp-month

```lisp
Function: (timestamp-month timestamp &key (timezone *default-timezone*))
```

Returns the month upon which the timestamp falls.

### timestamp-second

```lisp
Function: (timestamp-second timestamp &key (timezone *default-timezone*))
```

### timestamp-subtimezone

```lisp
Function: (timestamp-subtimezone timestamp timezone)
```

Return as multiple values the time zone as the number of seconds east of UTC, a boolean daylight-saving-p, and the customary abbreviation of the timezone.

### timestamp-to-universal

```lisp
Function: (timestamp-to-universal timestamp)
```

Return the UNIVERSAL-TIME corresponding to the [timestamp](#timestamp).
This is the date/time specified in `timestamp`
    encoded as the number of seconds since January 1st, 1900 12:00am
    UTC.

### timestamp-to-unix

```lisp
Function: (timestamp-to-unix timestamp)
```

Return the Unix time corresponding to the [timestamp](#timestamp).
This returns the date/time specified in `timestamp`
    encoded as the number of seconds since January 1st, 1970 12:00am
    UTC. It corresponds with the time received from the POSIX call
    `time()`.


### timestamp-week

No documentation found for `timestamp-week`

### timestamp-whole-year-difference

```lisp
Function: (timestamp-whole-year-difference time-a time-b)
```

Returns the number of whole years elapsed between time-a and time-b.

**Note:** This is useful for calculating anniversaries and birthdays.

### timestamp-year

```lisp
Function: (timestamp-year timestamp &key (timezone *default-timezone*))
```

Returns the cardinal year upon which the timestamp falls.

### timestamp/=

```lisp
Function: (timestamp/= &rest timestamps)
```

Returns T if no pair of timestamps is equal. Otherwise return NIL.

### timestamp<

```lisp
Function: (timestamp< &rest times)
```

### timestamp<=

```lisp
Function: (timestamp<= &rest times)
```

### timestamp=

```lisp
Function: (timestamp= &rest times)
```

### timestamp>

```lisp
Function: (timestamp> &rest times)
```

### timestamp>=

```lisp
Function: (timestamp>= &rest times)
```

### timezones-matching-subzone

```lisp
Function: (timezones-matching-subzone abbreviated-name timestamp)
```

Returns list of lists of active timezone, matched subzone and last transition time
   for timezones that have subzone matching specified abbreviated-name as of [timestamp](#timestamp) moment if provided.

### to-rfc1123-timestring

```lisp
Function: (to-rfc1123-timestring timestamp)
```

### to-rfc3339-timestring

```lisp
Function: (to-rfc3339-timestring timestamp)
```

### today

```lisp
Function: (today)
```

Produces a [timestamp](#timestamp) instance that corresponds to today's date,
which is the midnight of the current day in the UTC zone.

### universal-to-timestamp

```lisp
Function: (universal-to-timestamp universal &key (nsec 0))
```

Produces a [timestamp](#timestamp) instance from the provided universal time
universal. Universal time is defined in the Common Lisp
Specification as the number of seconds since 1900-01-01T00:00:00Z.

**Note:** Subsecond precision is not preserved.

### unix-to-timestamp

```lisp
Function: (unix-to-timestamp unix &key (nsec 0))
```

Produces a [timestamp](#timestamp) instance from the provided unix time unix. Unix time
is defined by POSIX as the number of seconds since 1970-01-01T00:00:00Z.

### with-decoded-timestamp

```lisp
Macro: (with-decoded-timestamp
        (&key nsec sec minute hour day month year day-of-week daylight-p
         timezone offset)
        timestamp &body forms)
```

This macro binds variables to the decoded elements of [timestamp](#timestamp). The timezone argument is used for decoding the timestamp, and is not bound by the macro. The value of day-of-week starts from 0 which means Sunday.

### zone-name

```lisp
Function: (zone-name zone)
```
