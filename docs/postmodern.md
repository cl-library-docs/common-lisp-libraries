# postmodern - PostgreSQL programming interace

Version: 1.30 (note: as of this writing, postmodern was at version 1.32 at github but only 1.30 at quicklisp)

Nickname: pomo

Repository: [marijnh/Postmodern - Github](https://github.com/marijnh/Postmodern)

*This page was possible due to the excellent [official documentation](https://marijnhaverbeke.nl/postmodern/postmodern.html).*

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/digikar99/common-lisp.readthedocs/issues).*

***

Postmodern is a Common Lisp library for interacting with [PostgreSQL
databases](https://postgresql.org). The focus has been on:

-   Efficient communication with the database server without need for
    foreign libraries.
-   Support for UTF-8 on Unicode-aware Lisp implementations
-   A syntax for mixing SQL and Lisp code
-   Convenient support for prepared statements and stored procedures
-   A metaclass for simple database-access objects

The biggest differences between this library and
[clsql](http://quickdocs.org/clsql/) or
[cl-dbi](https://github.com/fukamachi/cl-dbi) are that Postmodern has no
intention of being portable across different SQL implementations (it
embraces non-standard PostgreSQL features), and approaches extensions
like lispy SQL and database access objects in a quite different way.

## GETTING STARTED

### Starting the Postgres server

Follow the [installation instructions](https://www.postgresql.org/download/) to install Postgres.
Once done, you should have access to the `postgres` (the actual database server daemon), `pg_ctl`, and the `psql` commands on your command line / terminal.

Once done, [this page](https://www.postgresql.org/docs/current/server-start.html) elaborates
the process of starting the database server and any issues that may arise. (You can select the postgresql version from top of the page.)

You can also initialize and run multiple postgresql servers from different directories and ports

- Initialize the directory: `pg_ctl init -D postmodern # see \`pg_ctl --help`\ from the options`.
- Optionally, change `port` and `unix_socket_directories` from `postmodern/postgresql.conf`.
- `pg_ctl start -D postmodern` to start the server.

You should get a `server started` message; if not, the link above should help in debugging.
Proceed to the next section once you successfully start the server.

[This page](https://www.postgresql.org/docs/current/runtime-config-connection.html) elaborates on the configuration settings.

In addition, you can list the databases by using
`psql -p`*`PORT`*`-h`*`unix_socket_directories`*`-l`,
replacing the italicized arguments appropriately.

### Connecting to the Postgres server

We firstly connect to the default existing database. Create a new database for our
purposes, and then disconnect and reconnect to this database.

Assume the database server is started at a non-standard port of 8000 and username is `"username"`.

```lisp
CL-USER> (connect-toplevel "postgres" "username" "" "localhost" :port 8080)
; No value
CL-USER> (execute "create database testdb")
0
CL-USER> (disconnect-toplevel)
NIL
CL-USER> (connect-toplevel "testdb" "username" "" "localhost" :port 8080)
; No value
```
Connect-toplevel will maintain a single connection for the life of the session.

If you have multiple roles connecting to one or more databases, i.e. 1:many or
many:1, (in other words, changing connections) then with-connection form which establishes a connection with a lexical scope is more appropriate.


    (with-connection '("testdb" "foucault" "surveiller" "localhost")
      ...)

For example, if you are creating a database, you need to have established a connection
to a currently existing database (typically "postgres"). Assuming the foucault role
is a superuser and you want to stay in a development connection with your new database
afterwards, you would first use with-connection to connect to postgres, create the
database and then switch to connect-toplevel for development ease.


    (with-connection '("postgres" "foucault" "surveiller" "localhost")
      (create-database 'testdb :limit-public-access t
                         :comment "This database is for testing silly theories"))

    (connect-toplevel "testdb" "foucault" "surveiller" "localhost")


Note: (create-database) functionality is new to postmodern v. 1.32. Setting the
:limit-public-access parameter to t will block connections to that database from
anyone who you have not explicitly given permission (except other superusers).

A word about Postgresql connections. Postgresql connections are not lightweight
threads. They actually consume about 10 MB of memory per connection and Postgresql
can be tuned to limit the number of connections allowed at any one time. In
addition, any connections which require security (ssl or scram authentication)
will take additiona time and create more overhead.

If you have an application like a web app which will make many connections, you also
generally do not want to create and drop connections for every query. The usual solution
is to use connection pools so that the application is grabbing an already existing connection
and returning it to the pool when finished, saving connection time and memory.

To use postmodern's simple connection pooler, the with-connection call would look like:


    (with-connection '("testdb" "foucault" "surveiller" "localhost" :pooled-p t)
      ...)

The maximum number of connections in the pool is set in the special variable
\*max-pool-size\*, which defaults to nil (no maximum).


Other things you may want to take a look at with regards to connection include:

-   [database-connection](#database-connection)
-   [connect](#connect)
-   [\*default-use-ssl\*](#default-use-ssl)
-   [disconnect](#disconnect)
-   [connected-p](#connected-p)
-   [reconnect](#reconnect)
-   [\*database\*](#database)
-   [with-connection](#with-connection)
-   [call-with-connection](#call-with-connection)
-   [connect-toplevel](#connect-toplevel)
-   [disconnect-toplevel](#disconnect-toplevel)
-   [clear-connection-pool](#clear-connection-pool)
-   [\*max-pool-size\*](#max-pool-size)
-   [list-connections](#list-connections)

### Executing arbitrary database commands

[Query](#query) is the basic way to send queries to the database:

```lisp
CL-USER> (query "select 22, 'Folie et déraison', 4.5")
((22 "Folie et déraison" 9/2))
CL-USER> (query (:select 22 "Folie et déraison" 4.5))
((22 "Folie et déraison" 9/2))

```

In many contexts, query strings and lists starting with keywords can be used interchangeably. The lists will be compiled to SQL. The [S-SQL manual](https://marijnhaverbeke.nl/postmodern/s-sql.html) describes the syntax used by these expressions. Lisp values occurring in them are automatically escaped. In the above query, only constant values are used, but it is possible to transparently use run-time values as well:

```lisp
CL-USER> (defun database-powered-addition (a b)
           (query (:select (:+ a b)) :single))
DATABASE-POWERED-ADDITION
CL-USER> (database-powered-addition 1030 204)
1234
1
```

That last argument, `:single`, indicates that we want the result not as a list of lists (for the result rows), but as a single value, since we know that we are only selecting one value. See the documentation of [query](#query) for the other options.

You do not have to pull in the whole result of a query at once, you can also iterate over it with the [doquery](#doquery) macro.

The following things should be useful about querying:

-   [query](#query)
-   [execute](#execute)
-   [doquery](#doquery)
-   [prepare](#prepare)
-   [defprepared](#defprepared)
-   [defprepared-with-names](#defprepared-with-names)
-   [with-transaction](#with-transaction)
-   [commit-transaction](#commit-transaction)
-   [abort-transaction](#abort-transaction)
-   [with-savepoint](#with-savepoint)
-   [release-savepoint](#release-savepoint)
-   [rollback-savepoint](#rollback-savepoint)
-   [commit-hooks](#commit-hooks)
-   [abort-hooks](#abort-hooks)
-   [with-logical-transaction](#with-logical-transaction)
-   [abort-logical-transaction](#abort-logical-transaction)
-   [commit-logical-transaction](#commit-logical-transaction)
-   [\*current-logical-transaction\*](#current-logical-transaction)
-   [ensure-transaction](#ensure-transaction)
-   [with-schema](#with-schema)
-   [sequence-next](#sequence-next)
-   [coalesce](#coalesce)

### Database Access Class

You can work directly with the database or you can use a simple
database-access-class (aka [dao-class](#dao-class)) which would cover all the fields in a
row. This is what a database-access class looks like:

``` {.commonlisp}
(defclass points ()
  ((x :col-type integer :initarg :x
      :reader point-x)
   (y :col-type integer :initarg :y
      :reader point-y)
   (value :col-type integer :initarg :value
          :accessor value))
  (:metaclass dao-class)
  (:keys x y))
```

Once the class is defined, we create the table in the database:

```lisp
CL-USER> (dao-table-definition 'points)
"CREATE TABLE points (x INTEGER NOT NULL, y INTEGER NOT NULL, value INTEGER NOT NULL, PRIMARY KEY (x, y))"
CL-USER> (execute (dao-table-definition 'points))
0
```

In more complicated cases, you might want to `:create-table` directly. You can use [sql](#sql) to view the result of parsing the `form`.

```lisp
CL-USER> (sql (:create-table so-items
                             ((item-id :type integer)
                              (so-id :type (or integer db-null) :references ((so-headers id)))
                              (product-id :type (or integer db-null))
                              (qty :type (or integer db-null))
                              (net-price :type (or numeric db-null)))
                             (:primary-key item-id so-id)))
"CREATE TABLE so_items (item_id INTEGER NOT NULL, so_id INTEGER REFERENCES so_headers(id) MATCH SIMPLE ON DELETE RESTRICT ON UPDATE RESTRICT, product_id INTEGER, qty INTEGER, net_price NUMERIC, PRIMARY KEY (item_id, so_id))"
```

(Note that you'd need to execute this form using [query](#query) or [execute](#execute). Also see [this](https://marijnhaverbeke.nl/postmodern/create-tables.html) for details and examples on using S-SQL for creating tables.)

You can use [insert-dao](#insert-dao) for inserting DAO-objects into the database. (Of course, you can use `query` to do all this using the usual SQL syntax.)

```lisp
CL-USER> (insert-dao (make-instance 'points :x 0 :y 1 :value 10))
#<POINTS {10145AC1D3}>
CL-USER> (insert-dao (make-instance 'points :x 1 :y 0 :value 15))
#<POINTS {10145B5EA3}>
CL-USER> (select-dao 'country)
(#<COUNTRY {101C5F3103}> #<COUNTRY {101C5F3883}>)
2
CL-USER> (select-dao 'points)
(#<POINTS {1017B9BB93}> #<POINTS {1017B9BC23}>)
2
CL-USER> (query (:select '* :from 'points))
((0 1 10) (1 0 15))
2
```

Finally, you can use [update-dao](#update-dao) to, well, update the database entry:

```lisp
CL-USER> (let ((1-0 (get-dao 'points 1 0)))
           (setf (value 1-0) 20)
           (update-dao 1-0))
#<POINTS {101F1628E3}>
CL-USER> (query (:select '* :from 'points))
((0 1 10) (1 0 20))
2
```

Here are the other useful constructs related to database access objects:

-   [dao-class](#dao-class)
-   [dao-keys](#dao-keys)
-   [dao-exists-p](#dao-exists-p)
-   [make-dao](#make-dao)
-   [define-dao-finalization](#define-dao-finalization)
-   [get-dao](#get-dao)
-   [select-dao](#select-dao)
-   [do-select-dao](#do-select-dao)
-   [query-dao](#query-dao)
-   [do-query-dao](#do-query-dao)
-   [\*ignore-unknown-columns\*](#ignore-unknown-columns)
-   [insert-dao](#insert-dao)
-   [update-dao](#update-dao)
-   [save-dao](#save-dao)
-   [save-dao/transaction](#save-dao/transaction)
-   [upsert-dao](#upsert-dao)
-   [delete-dao](#delete-dao)
-   [dao-table-name](#dao-table-name)
-   [dao-table-definition](#dao-table-definition)
-   [with-column-writers](#with-column-writers)


### S-SQL

Finally, here is some more demonstration of S-SQL syntax:
``` {.commonlisp}
(sql (:select 'relname :from 'pg-catalog.pg-class
      :inner-join 'pg-catalog.pg-namespace :on (:= 'relnamespace 'pg-namespace.oid)
      :where (:and (:= 'relkind "r")
                   (:not-in 'nspname (:set "pg_catalog" "pg_toast"))
                   (:pg-catalog.pg-table-is-visible 'pg-class.oid))))
;; => "(SELECT relname FROM pg_catalog.pg_class
;;      INNER JOIN pg_catalog.pg_namespace ON (relnamespace = pg_namespace.oid)
;;      WHERE ((relkind = 'r') and (nspname NOT IN ('pg_catalog', 'pg_toast'))
;;             and pg_catalog.pg_table_is_visible(pg_class.oid)))"
```

Lists starting with keywords are used to express SQL
commands and operators while lists starting with something else will be
evaluated and then inserted into the query. Quoted symbols name columns
or tables (keywords can also be used but might introduce ambiguities).
The syntax supports subqueries, multiple joins, stored procedures, etc.
See the [S-SQL reference manual](https://marijnhaverbeke.nl/postmodern/s-sql.html) for a complete treatment.

[TODO] Add more S-SQL examples. Compare them with SxQL as well.

### Prepared statements

-   [\*allow-overwriting-prepared-statements\*](#allow-overwriting-prepared-statements)
-   [prepared-statement-exists-p](#prepared-statement-exists-p)
-   [list-prepared-statements](#list-prepared-statements)
-   [drop-prepared-statement](#drop-prepared-statement)
-   [list-postmodern-prepared-statements](#list-postmodern-prepared-statements)
-   [find-postgresql-prepared-statement](#find-postgresql-prepared-statement)
-   [find-postmodern-prepared-statement](#find-postmodern-prepared-statement)
-   [reset-prepared-statement](#reset-prepared-statement)
-   [get-pid](#get-pid)
-   [get-pid-from-postmodern](#get-pid-from-postmodern)
-   [cancel-backend](#cancel-backend)
-   [terminate-backend](#terminate-backend)

Example Usage:

```lisp
CL-USER> (funcall (prepare (:select '* :from 'points
                                    :where (:= 'x '$1)))
                  0)
((0 1 10))
1
```

-   [prepare](#prepare)
-   [defprepared](#defprepared)
-   [defprepared-with-names](#defprepared-with-names)

### Migration

The meaning of the term migration depends context. People can talk about migrating
from Oracle to Postgresql or to Mssql or Mysql. In that context, migration means
changing database structure and functions from one database implementation to another.

To developers, the term migration normally means tracking and managing version
changes of the database structure in the development process. This is often called
schema migration. Of course development often continues after software has gone into
production, in which case "migration' not only needs to deal with version controls
of the database structure, but also how to ensure that such changes of the database
do not result in lost production data. For example, renaming a column in a database
table is a simple one command operation in development but at least four commands
if there is actually data in the column.

There are two different approaches taken to schema migration. The first and more
typical approach is is writing scripts to manage changes, both forward and back.
The second approach is to generate diff snapshots and determine the changes needed
to move from one snapshot to another. Both have their pluses and minuses, particularly
when it comes to how to manage the data that already exists in the database.

The script writing approach requires the developer to write both the sql commands
necessary to make the desired change and the requisite sql commands to undo that
change. Typically these scripts are then saved in .sql files in a migration directory
and a migration library is used to track dependencies which can get complicated if
there are more than one developer on the team.

Undoing migrations (sometimes called rollbacks) is difficult if production data
exists on the database. There are schools of thought among DBAs that rollbacks
should never be allowed because of the danger of losing critical production data.

It may be obvious, but it is a good reminder that any migration should start with
creating a backup which has been tested.

[https://github.com/madnificent/database-migrations](https://github.com/madnificent/database-migrations)
provides some simple migration tools for postmodern.

### Other useful constructs

-   **Inspecting the database**
    -   [list-tables](#list-tables)
    -   [list-tables-in-schema](#list-tables-in-schema)
    -   [table-exists-p](#table-exists-p)
    -   [table-description](#table-description)
    -   [list-sequences](#list-sequences)
    -   [sequence-exists-p](#sequence-exists-p)
    -   [list-views](#list-views)
    -   [view-exists-p](#view-exists-p)
    -   [list-schemata](#list-schemata)
    -   [schema-exist-p](#schema-exist-p)
    -   [schema-exists-p](#schema-exists-p)
    -   [database-version](#database-version)
    -   [num-records-in-database](#num-records-in-database)
    -   [current-database](#current-database)
    -   [database-exists-p](#database-exists-p)
    -   [database-size](#database-size)
    -   [list-databases](#list-databases)
    -   [list-schemas](#list-schemas)
    -   [list-tablespaces](#list-tablespaces)
    -   [list-available-types](#list-available-types)
    -   [list-table-sizes](#list-table-sizes)
    -   [table-size](#table-size)
    -   [more-table-info](#more-table-info)
    -   [list-columns](#list-columns)
    -   [list-columns-with-types](#list-columns-with-types)
    -   [column-exists-p](#column-exists-p)
    -   [describe-views](#describe-views)
    -   [list-database-functions](#list-database-functions)
    -   [list-indices](#list-indices)
    -   [list-table-indices](#list-table-indices)
    -   [index-exists-p](#index-exists-p)
    -   [list-indexed-column-and-attributes](#list-indexed-column-and-attributes)
    -   [list-index-definitions](#list-index-definitions)
    -   [find-primary-key-info](#find-primary-key-info)
    -   [list-foreign-keys](#list-foreign-keys)
    -   [list-unique-or-primary-constraints](#list-unique-or-primary-constraints)
    -   [list-all-constraints](#list-all-constraints)
    -   [describe-constraint](#describe-constraint)
    -   [describe-foreign-key-constraints](#describe-foreign-key-constraints)
    -   [list-triggers](#list-triggers)
    -   [list-detailed-triggers](#list-detailed-triggers)
    -   [list-database-users](#list-database-users)
    -   [list-roles](#list-roles)
    -   [list-available-extensions](#list-available-extensions)
    -   [list-installed-extensions](#list-installed-extensions)
    -   [change-toplevel-database](#change-toplevel-database)
-   **Table definition and creation**
    -   [deftable](#deftable)
    -   [!dao-def](#dao-def)
    -   [!index](#index)
    -   [!unique-index](#unique-index)
    -   [!foreign](#foreign)
    -   [!unique](#unique)
    -   [create-table](#create-table)
    -   [create-all-tables](#create-all-tables)
    -   [create-package-tables](#create-package-tables)
    -   [\*table-name\*](#table-name)
    -   [\*table-symbol\*](#table-symbol)
-   **Schemata**
    -   [create-schema](#create-schema)
    -   [drop-schema](#drop-schema)
    -   [get-search-path](#get-search-path)
    -   [set-search-path](#set-search-path)
    -   [split-fully-qualified-table-name](#split-fully-qualified-table-name)
-   **Database Health Measurements**
    -   [cache-hit-ratio](#cache-hit-ratio)
    -   [bloat-measurement](#bloat-measurement)
    -   [unused-indexes](#unused-indexes)
    -   [check-query-performance](#check-query-performance)
-   **Miscellaneous Utility Functions**
    -   [execute-file](#execute-file)

## CONFIGURATION VARIABLES

### \*allow-overwriting-prepared-statements\*

```lisp
Variable
```

When set to t, ensured-prepared will overwrite prepared statements
having the same name if the query statement itself in the postmodern
meta connection is different than the query statement provided to
ensure-prepared.

### \*current-logical-transaction\*

```lisp
Variable
```

This is bound to the current transaction-handle or savepoint-handle
instance representing the innermost open logical transaction.

### \*database\*

```lisp
Variable
```

Special variable holding the current database. Most functions and macros
operating on a database assume this binds to a connected database.

### \*default-use-ssl\*

```lisp
Variable
```

The default for connect's use-ssl argument. This starts at :no. If you
set it to anything else, be sure to also load the CL+SSL library.

### \*escape-sql-names-p\*

```lisp
Variable
```

Setting this to T will make S-SQL add double quotes around
identifiers in queries. Setting it :auto will turn on this behaviour
only for reserved words. Setting it to :literal will cause to-sql-name to
escape reserved words,but will not make other changes such as changing
forward slash to underscore.

### \*ignore-unknown-columns\*

```lisp
Variable
```

Normally, when get-dao, select-dao, or query-dao finds a column in the
database that's not in the DAO class, it will raise an error. Setting
this variable to a non-NIL will cause it to simply ignore the unknown
column.

### \*isolation-level\*

### \*max-pool-size\*

```lisp
Variable
```

Set the maximum amount of connections kept in a single connection pool,
where a pool consists of all the stored connections with the exact same
connect arguments. Defaults to NIL, which means there is no maximum.

### \*table-name\*,

```lisp
Variable
```

These variables are bound to the relevant name and symbol while the
forms of a table definition are evaluated. Can be used to define
shorthands like the ones below.

### \*table-symbol\*

```lisp
Variable
```

Used inside [deftable](#deftable) to find the symbol naming the table being defined.

## FUNCTIONS AND MACROS

### !dao-def

```lisp
Function: (!dao-def)
```

Should only be used inside deftable's body. Adds the result of calling
dao-table-definition on **table-symbol** to the definition.

### !foreign

```lisp
Function: (!foreign target fields &rest
 target-fields/on-delete/on-update/deferrable/initially-deferred)
```

Add a foreign key to the table being defined. target-table is the
referenced table. columns is a list of column names or single name in
this table, and, if the columns have different names in the referenced
table, target-columns must be another list of column names or single
column name of the target-table, or :primary-key to denote the column(s)
of the target-table's primary key as referenced column(s).

The on-delete and on-update arguments can be used to specify ON DELETE
and ON UPDATE actions, as per the keywords allowed in create-table. In
addition, the deferrable and initially-deferred arguments can be used to
indicate whether constraint checking can be deferred until the current
transaction completed, and whether this should be done by default. Note
that none of these are really &key arguments, but rather are picked out
of a &rest arg at runtime, so that they can be specified even when
target-columns is not given.

### !index

```lisp
Function: (!index &rest fields)
```

Define an index on the table being defined. The columns can be given as
symbols or strings.

### !unique

```lisp
Function: (!unique target-fields &key deferrable initially-deferred)
```

Constrains one or more columns to only contain unique (combinations of)
values, with deferrable and initially-deferred defined as in !foreign

### !unique-index

```lisp
Function: (!unique-index &rest fields)
```
Used inside a [deftable](#deftable) form. Define a unique index on the defined table.


### abort-hooks

```lisp
Generic Function: (abort-hooks object)
Generic Function: (setf (abort-hooks object) ...)
```

An accessor for the transaction or savepoint's list of commit hooks,
each of which should be a function with no required arguments. These
functions will be executed when a transaction is committed or a
savepoint released.

### abort-logical-transaction

```lisp
Generic Function: (abort-logical-transaction savepoint)
```

Roll back the given logical transaction, regardless of whether it is an
actual transaction or a savepoint.

### abort-transaction

```lisp
Function: (abort-transaction transaction)
```

Roll back the given transaction.

### bloat-measurement

```lisp
Function: (bloat-measurement)
```

→ list

Bloat measurement of unvacuumed dead tuples. Borrowed from:
<https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/>
who borrowed it from
<https://github.com/heroku/heroku-pg-extras/tree/master/commands>.

### cache-hit-ratio

```lisp
Function: (cache-hit-ratio)
```

→ list

The cache hit ratio shows data on serving the data from memory compared
to how often you have to go to disk. This function returns a list of
heapblocks read from disk, heapblocks hit from memory and the ratio of
heapblocks hit from memory / total heapblocks hit. Borrowed from:
<https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/>

### call-with-connection

```lisp
Function: (call-with-connection spec thunk)
```

The functional backend to with-connection. Binds \*database\* to a new
connection as specified by spec, which should be a list that connect can
be applied to, and runs the zero-argument function given as second
argument in the new environment. When the function returns or throws,
the new connection is disconnected.

### cancel-backend

```lisp
Function: (cancel-backend pid &optional (database *database*))
```

Polite way of terminating a query at the database (as opposed to calling
close-database). Slower than (terminate-backend pid) and does not always
work.

### change-toplevel-database

```lisp
Function: (change-toplevel-database new-database user password host)
```

→ string

Just changes the database assuming you are using a toplevel connection.
Recommended only for development work. Returns the name of the newly
connected database as a string.

### check-query-performance

```lisp
Function: (check-query-performance &optional (ob nil) (num-calls 100) (limit 20))
```

→ list

This function requires that postgresql extension pg\_stat\_statements
must be loaded via shared\_preload\_libraries. It is borrowed from
<https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/>.
Optional parameters OB allow order-by to be 'calls', 'total-time',
'rows-per' or 'time-per', defaulting to time-per. num-calls to require
that the number of calls exceeds a certain threshold, and limit to limit
the number of rows returned. It returns a list of lists, each row
containing the query, number of calls, total\_time, total\_time/calls,
stddev\_time, rows, rows/calls and the cache hit percentage.

### clear-connection-pool

```lisp
Function: (clear-connection-pool)
```

Disconnect and remove all connections from the connection pools.

### coalesce

```lisp
Function: (coalesce &rest args)
```

→ value

Returns the first non-NIL, non-NULL (as in :null) argument, or NIL if
none are present. Useful for providing a fall-back value for the result
of a query, or, when given only one argument, for transforming :nulls to
NIL.

### column-exists-p

```lisp
Function: (column-exists-p table-name column-name)
```

→ boolean

Determine if a particular column exists. Table name and column-name can
be either strings or symbols.

### commit-hooks

```lisp
Generic Function: (commit-hooks object)
```

An accessor for the transaction or savepoint's list of commit hooks,
each of which should be a function with no required arguments. These
functions will be executed when a transaction is committed or a
savepoint released.

### commit-logical-transaction

```lisp
Generic Function: (commit-logical-transaction savepoint)
```

[TODO] This symbol is not exported.

Commit the given logical transaction, regardless of whether it is an
actual transaction or a savepoint.

### commit-transaction

```lisp
Function: (commit-transaction transaction)
```

Commit the given transaction.

### connect

```lisp
Function: (connect database-name user password host &key (port 5432) pooled-p
 (use-ssl *default-use-ssl*) (service postgres))
```

→ database-connection

Create a new database connection for the given user and the database.
Port will default to 5432, which is where most PostgreSQL servers are
running. If pooled-p is T, a connection will be taken from a pool of
connections of this type, if one is available there, and when the
connection is disconnected it will be put back into this pool instead.
use-ssl can be :no, :yes, or :try, as in open-database, and defaults to
the value of \*default-use-ssl\*.

### connect-toplevel

```lisp
Function: (connect-toplevel database-name user password host &key (port 5432)
 (use-ssl *default-use-ssl*))
```

Bind the \*database\* to a new connection. Use this if you only need one
connection, or if you want a connection for debugging from the REPL.

### connected-p

```lisp
Function: (connected-p database)
```

→ boolean

Returns a boolean indicating whether the given connection is still
connected to the server.

### create-all-tables

```lisp
Function: (create-all-tables)
```

Creates all defined tables.

### create-package-tables

```lisp
Function: (create-package-tables package)
```

Creates all tables identified by symbols interned in the given package.

### create-schema

```lisp
Function: (create-schema schema)
```

Creates a new schema. Raises an error if the schema is already exists.

### create-table

```lisp
Function: (create-table name)
```

Creates the table identified by symbol by executing all forms in its
definition.

### current-database

```lisp
Function: (current-database)
```

→ string

Returns the string name of the current database.

### dao-class

```lisp
: (dao-class)
```

Postmodern contains a simple system for defining CLOS classes that
represent rows in the database. This is not intended as a full-fledged
object-relational magic system ― while serious ORM systems have their
place, they are notoriously hard to get right, and are outside of the
scope of a humble SQL library like this.

At the heart of Postmodern's DAO system is the dao-class metaclass. It
allows you to define classes for your database-access objects as regular
CLOS classes. Some of the slots in these classes will refer to columns
in the database. To specify that a slot refers to a column, give it a
:col-type option containing an S-SQL type expression (useful if you want
to be able to derive a table definition from the class definition), or
simply a :column option with value T. Such slots can also take a
:col-default option, used to provide a database-side default value as an
S-SQL expression. You can use the :col-name initarg (whose unevaluated
value will be passed to to-sql-name) to specify the slot's column's
name.

DAO class definitions support two extra class options: :table-name to
give the name of the table that the class refers to (defaults to the
class name), and :keys to provide a set of primary keys for the table.
When no primary keys are defined, operations such as update-dao and
get-dao will not work.

IMPORTANT: Class finalization for a dao class instance are wrapped with
a thread lock. However, any time you are using threads and a class that
inherits from other classes, you should ensure that classes are
finalized before you start generating threads that create new instances
of that class.

Simple example:

``` {.commonlisp}
(defclass user ()
  ((name :col-type string :initarg :name :accessor user-name)
   (creditcard :col-type (or db-null integer) :initarg :card :col-default :null)
   (score :col-type bigint :col-default 0 :accessor user-score))
  (:metaclass dao-class)
  (:keys name))
```

The (or db-null integer) form is used to indicate a column can have NULL
values.

When inheriting from DAO classes, a subclass' set of columns also
contains all the columns of its superclasses. The primary key for such a
class is the union of its own keys and all the keys from its
superclasses. Classes inheriting from DAO classes should probably always
use the dao-class metaclass themselves.

When a DAO is created with make-instance, the :fetch-defaults keyword
argument can be passed, which, when T, will cause a query to fetch the
default values for all slots that refers to columns with defaults and
were not bound through initargs. In some cases, such as serial columns,
which have an implicit default, this will not work. You can work around
this by creating your own sequence, e.g. "my\_sequence", and defining a
(:nextval "my\_sequence") default.

Finally, DAO class slots can have an option :ghost t to specify them as
ghost slots. These are selected when retrieving instances, but not
written when updating or inserting, or even included in the table
definition. The only known use for this to date is for creating the
table with (oids=true), and specify a slot like this:

``` {.commonlisp}
(oid :col-type integer :ghost t :accessor get-oid)
```

### dao-exists-p

```lisp
Generic Function: (dao-exists-p dao)
```

→ boolean

Test whether a row with the same primary key as the given dao exists in
the database. Will also return NIL when any of the key slots in the
object are unbound.

### dao-keys

```lisp
Generic Function: (dao-keys class)
```

→ list

Returns list of slot names that are the primary key of DAO class. This
is likely interesting if you have primary keys which are composed of
more than one slot. Pay careful attention to situations where the
primary key not only has more than one column, but they are actually in
a different order than they are in the database table itself. You can
check this with the find-primary-key-info function.

``` {.commonlisp}
(pomo:find-primary-key-info "country1")

(("name" "text") ("id" "integer"))
```
→ list

Returns list of values that are the primary key of dao.

### dao-table-definition

```lisp
Function: (dao-table-definition table)
```

→ string

Given a DAO class, or the name of one, this will produce an SQL query
string with a definition of the table. This is just the bare simple
definition, so if you need any extra indices or or constraints, you'll
have to write your own queries to add them, in which case look to
s-sql's create-table function.

### dao-table-name

```lisp
Function: (dao-table-name class)
```

→ string

Get the name of the table associated with the given DAO class (or symbol
naming such a class).

### database-connection

```lisp
: (database-connection)
```

Objects of this type represent database connections.

### database-error


```lisp
Condition
```

This is the condition type that will be used to
signal virtually all database-related errors (though in some cases
socket errors may be raised when a connection fails on the IP
level).

<u>**Direct Slots**</u>

**cl-postgres::error-code**
```lisp
Initargs: :CODE
```
**cl-postgres::message**
```lisp
Initargs: :MESSAGE
Readers: CL-POSTGRES:DATABASE-ERROR-MESSAGE
```
**cl-postgres::detail**
```lisp
Initargs: :DETAIL
```
**cl-postgres::hint**
```lisp
Initargs: :HINT
```
**cl-postgres::context**
```lisp
Initargs: :CONTEXT
```
**cl-postgres::query**
```lisp
```
**position**
```lisp
Initargs: :POSITION
```
**cl-postgres::cause**
```lisp
Initargs: :CAUSE
```
### database-error-cause

```lisp
Generic Function: (database-error-cause condition)
```


### database-error-code

```lisp
Generic Function: (database-error-code condition)
```


### database-error-constraint-name

```lisp
Function: (database-error-constraint-name err)
```
Given a database-error for an integrity violation, will attempt to
extract the constraint name.

### database-error-detail

```lisp
Generic Function: (database-error-detail condition)
```


### database-error-extract-name

```lisp
Function: (database-error-extract-name err)
```
Given a database-error, will extract the critical name from the error message.

### database-error-message

```lisp
Generic Function: (database-error-message condition)
```


### database-error-query

```lisp
Generic Function: (database-error-query condition)
```

### database-exists-p

```lisp
Function: (database-exists-p database-name)
```

→ boolean

Checks to see if a particular database exists.

### database-size

```lisp
Function: (database-size &optional (name nil))
```

→ list

Given the name of a database, will return the name, a pretty-print
string of the size of the database and the size in bytes. If a database
name is not provided, it will return the result for the currently
connected database.

### database-version

```lisp
Function: (database-version)
```

→ string

Returns the version of the current postgresql database.

### define-dao-finalization

```lisp
Macro: (define-dao-finalization ((dao-name class) &rest keyword-args) &body body)
```

Create an :around-method for make-dao. The body is executed in a lexical
environment where dao-name is bound to a freshly created and inserted
DAO. The representation of the DAO in the database is then updated to
reflect changes that body might have introduced. Useful for processing
values of slots with the type serial, which are unknown before
insert-dao.

### defprepared

```lisp
Macro: (defprepared name query &optional (format rows))
```

→ function

This is the macro-style variant of prepare. It is like prepare, but
gives the function a name which now becomes a top-level function for the
prepared statement. The name should not be quoted or a string.

### defprepared-with-names

```lisp
Macro: (defprepared-with-names name (&rest args) (query &rest query-args) &optional
 (format rows))
```

Like defprepared, but allows to specify names of the function arguments
as well as arguments supplied to the query.

``` {.commonlisp}
(defprepared-with-names user-messages (user &key (limit 10))
  ("select * from messages
    where user_id = $1
    order by date desc
    limit $2" (user-id user) limit)
  :plists)
```

### deftable

```lisp
Macro: (deftable name &body definitions)
```

Define a table. name can be either a symbol or a (symbol string) list.
In the first case, the table name is derived from the symbol's name by
S-SQL's rules. In the second case, the name is given explicitly. The
body of definitions can contain anything that evaluates to a string, as
well as S-SQL expressions. The variables \*table-name\* and
\*table-symbol\* are bound to the relevant values in the body. Note that
the evaluation of the definition is ordered, so you'll generally want to
create your table first and then define indices on it.

### delete-dao

```lisp
Generic Function: (delete-dao dao)
```

Delete the given dao from the database.

### describe-constraint

```lisp
Function: (describe-constraint table-name constraint-name)
```

→ list

Return a list of alists of the descriptions a particular constraint
given the table-name and the constraint name using the
information\_schema table.

### describe-foreign-key-constraints

```lisp
Function: (describe-foreign-key-constraints)
```

→ list

Generates a list of lists of information on the foreign key constraints

### describe-views

```lisp
Function: (describe-views &optional (schema public))
```

→ list

Describe the current views in the specified schema. Defaults to public
schema.

### disconnect

```lisp
Generic Function: (disconnect database)
```

Disconnects a normal database connection, or moves a pooled connection
into the pool.

### disconnect-toplevel

```lisp
Function: (disconnect-toplevel)
```

Disconnect the \*database\*.

### do-query-dao

```lisp
Macro: (do-query-dao ((type type-var) query) &body body)
```

→ list

Like query-dao, but iterates over the results rather than returning
them. For each matching DAO, body is evaluated with type-var bound to
the instance.

``` {.commonlisp}
(do-query-dao (('user user) (:order-by (:select '* :from 'user :where (:> 'score 10000)) 'name))
  (pushnew user high-scorers))
```

### do-select-dao

```lisp
Macro: (do-select-dao ((type type-var) &optional (test) &rest ordering) &body body)
```

Like select-dao, but iterates over the results rather than returning
them. For each matching DAO, body is evaluated with type-var bound to
the DAO instance.

``` {.commonlisp}
(do-select-dao (('user user) (:> 'score 10000) 'name)
  (pushnew user high-scorers))
```

### doquery

```lisp
Macro: (doquery query (&rest names) &body body)
```

Execute the given query (a string or a list starting with a keyword),
iterating over the rows in the result. The body will be executed with
the values in the row bound to the symbols given in names. To iterate
over a parameterised query, one can specify a list whose car is the
query, and whose cdr contains the arguments. For example:

``` {.commonlisp}
(doquery (:select 'name 'score :from 'scores) (n s)
  (incf (gethash n *scores*) s))

(doquery ((:select 'name :from 'scores :where (:> 'score '$1)) 100) (name)
  (print name))
```

### double-precision

### drop-index

```lisp
Function: (drop-index name &key concurrently if-exists cascade)
```
Drop an index. Available keys are :concurrently, :if-exists, and :cascade.

### drop-prepared-statement

```lisp
Function: (drop-prepared-statement name &key (location both) (database *database*)
 (remove-function t))
```

Prepared statements are stored both in the meta slot in the postmodern
connection and in postgresql session information. If you know the
prepared statement name, you can delete the prepared statement from both
locations (the default behavior), just from postmodern (passing
:postmodern to the location key parameter) or just from postgresql
(passing :postgresql to the location key parameter). If you pass the
name 'All' as the statement name, it will delete all prepared
statements. The statement name can be a string or quoted symbol.

### drop-schema

```lisp
Function: (drop-schema schema &key (if-exists nil) (cascade nil))
```

Removes a schema. Accepts :if-exists and/or :cascade arguments like
:drop-table.

### drop-sequence

```lisp
Function: (drop-sequence name &key if-exists cascade)
```
Drop a sequence. Name should be quoted. Available key parameters are :if-exists and :cascade

### drop-table

```lisp
Function: (drop-table name &key if-exists cascade)
```
Drop a table. Available additional key parameters are :if-exists and :cascade.

### ensure-transaction

```lisp
Macro: (ensure-transaction &body body)
```

Ensures that body is executed within a transaction, but does not begin a
new transaction if one is already in progress.

### ensure-transaction-with-isolation-level

```lisp
Macro: (ensure-transaction-with-isolation-level isolation-level &body body)
```
Executes body within a with-transaction form if and only if no
transaction is already in progress. This adds the ability to specify an isolatin
level other than the current default

### execute

```lisp
Macro: (execute query &rest args)
```

Like query called with format :none. Returns the amount of affected rows
as its first returned value. (Also returns this amount as the second
returned value, but use of this is deprecated.)

### execute-file

```lisp
Function: (execute-file pathname &optional (print nil))
```

This function will execute sql queries stored in a file. Each sql
statement in the file will be run independently, but if one statement
fails, subsequent query statements will not be run, but any statement
prior to the failing statement will have been commited.

If you want the standard transction treatment such that all statements
succeed or no statement succeeds, then ensure that the file starts with
a "begin transaction" statement and finishes with an "end transaction"
statement. See the test file test-execute-file-broken-transaction.sql as
an example.

For debugging purposes, if the optional print parameter is set to t,
format will print the count of the query and the query to the REPL.

IMPORTANT NOTE: This utility function assumes that the file containing
the sql queries can be trusted and bypasses the normal postmodern
parameterization of queries.

### find-postgresql-prepared-statement

```lisp
Function: (find-postgresql-prepared-statement name)
```

→ string

Returns the specified named prepared statement (if any) that postgresql
has for this session.

### find-postmodern-prepared-statement

```lisp
Function: (find-postmodern-prepared-statement name)
```

→ string

Returns the specified named prepared statement (if any) that postmodern
has put in the meta slot in the connection. Note that this is the
statement itself, not the name.

### find-primary-key-info

```lisp
Function: (find-primary-key-info table &optional (just-key nil))
```

→ list

Returns a list of two strings. First the column name of the primary key
of the table and second the string name for the datatype. Optionally,
just-key can be set to t and it will return just the column name of the
primary key as a string.

### get-dao

```lisp
Generic Function: (get-dao type &rest args)
```

→ dao

Select the DAO object from the row that has the given primary key
values, or NIL if no such row exists. Objects created by this function
will have initialize-instance called on them (after loading in the
values from the database) without any arguments ― even :default-initargs
are skipped. The same goes for select-dao and query-dao.

### get-pid

```lisp
Function: (get-pid)
```

→ integer

Get the process id used by postgresql for this connection.

### get-pid-from-postmodern

```lisp
Function: (get-pid-from-postmodern)
```

→ integer

Get the process id used by postgresql for this connection, but get it
from the postmodern connection parameters.

### get-search-path

```lisp
Function: (get-search-path)
```

Retrieve the current search path.

### index-exists-p

```lisp
Function: (index-exists-p index-name)
```

→ boolean

Tests whether an index with the given name exists. The name can be
either a string or a symbol.

### insert-dao

```lisp
Generic Function: (insert-dao dao)
```

→ dao

Insert the given dao into the database. Column slots of the object which
are unbound implies the database defaults. Hence, if these columns has
no defaults defined in the database, the the insertion of the dao will
be failed. (This feature only works on PostgreSQL 8.2 and up.)

### list-all-constraints

```lisp
Function: (list-all-constraints table-name &optional (strings-p))
```

→ list

Users information\_schema to list all the constraints in a table.
Table-name can be either a string or quoted.

### list-available-extensions

```lisp
Function: (list-available-extensions)
```

→ list

List the postgresql extensions which are available in the system to the
currently connected database. The extensions may or may not be
installed.
→ list

Lists extensions that are available to be installed in the database.
Returns a list of lists where each sublist has the name of the
extension, the default version, the installed version (if any) and a
comment string.

### list-available-types

```lisp
Function: (list-available-types)
```

→ list

List the available types in this postgresql version.

### list-columns

```lisp
Function: (list-columns table-name)
```

→ list

Returns a list of strings of just the column names in a table. Pulls
info from the postmodern table-description function rather than
directly.

### list-columns-with-types

```lisp
Function: (list-columns-with-types table-name)
```

→ list

Return a list of (name type) lists for the fields of a table. Goes
directly to the pg-catalog tables.

### list-connections

```lisp
Function: (list-connections)
```

→ list

List the current postgresql connections to the currently connected
database.

### list-database-functions

```lisp
Function: (list-database-functions)
```

→ list

Returns a list of the functions in the database from the
information\_schema.

### list-database-users

```lisp
Function: (list-database-users)
```

→ list

List database users.

### list-databases

```lisp
Function: (list-databases &key (order-by-size nil) (size t))
```

→ list

Returns a list of lists where each sub-list contains the name of the
database, a pretty-print string of the size of that database and the
size in bytes. The default order is by database name. Pass t as a
parameter to :order-by-size for order by size. Setting size to nil will
return just the database names in a single list ordered by name. This
function excludes the template databases

### list-detailed-triggers

```lisp
Function: (list-detailed-triggers)
```

→ list

List detailed information on the triggers from the information\_schema
table.

### list-foreign-keys

```lisp
Function: (list-foreign-keys table schema)
```

→ list

Returns a list of sublists of foreign key info in the form of
'((constraint-name local-table local-table-column foreign-table-name
foreign-column-name))

### list-index-definitions

```lisp
Function: (list-index-definitions table-name)
```

→ list

Returns a list of the definitions used to create the current indexes for
the table

### list-indexed-column-and-attributes

```lisp
Function: (list-indexed-column-and-attributes table-name)
```

→ list

List the indexed columns and their attributes in a table. Includes
primary key.

### list-indices

```lisp
Function: (list-indices &optional strings-p)
```

→ list

Return a list of the indexs in a database. Turn them into keywords if
strings-p is not true.

### list-installed-extensions

```lisp
Function: (list-installed-extensions)
```

→ list

List the postgresql extensions which are installed in the currently
connected database.
→ list

Return a list of the installed extension

### list-postmodern-prepared-statements

```lisp
Function: (list-postmodern-prepared-statements &optional (names-only nil))
```

→ list

List the prepared statements that postmodern has put in the meta slot in
the connection. It will return a list of alists of form: ((:NAME .
SNY24) (:STATEMENT . (SELECT name, salary FROM employee WHERE (city =
\$1))) (:PREPARE-TIME . \#&lt;TIMESTAMP 25-11-2018T15:36:43,385&gt;)
(:PARAMETER-TYPES . text) (:FROM-SQL)

If the names-only parameter is set to t, it will only return a list of
the names of the prepared statements.

### list-prepared-statements

```lisp
: (list-prepared-statements (&optional names-only))
```

→ list

This is syntactic sugar. It runs a query that lists the prepared
statements in the session in which the function is run. If the
names-only parameter is set to t, it will only return a list of the
names of the prepared statements.

### list-roles

```lisp
Function: (list-roles &optional (lt nil))
```

→ list

Returns a list of alists of rolenames, role attributes and membership in
roles. See
<https://www.postgresql.org/docs/current/role-membership.html> for an
explanation. Optionally passing :alists or :plists can be used to set
the return list types to :alists or :plists. This is the same as the
psql function \du.

### list-schemas

```lisp
Function: (list-schemas)
```

→ list

List schemas in the current database, excluding the pg\_\* system
schemas.

### list-schemata

```lisp
Function: (list-schemata)
```

→ list

Returns list of the user defined schemata (as strings) and the quantity
of existing schemata.

### list-sequences

```lisp
Function: (list-sequences &optional strings-p)
```

→ list

Returns a list of the sequences in the current database. When strings-p
is T, the names will be given as strings, otherwise as keywords.

### list-table-indices

```lisp
Function: (list-table-indices table-name &optional strings-p)
```

→ list

List the index names and the related columns in a table. Returns a list
of alists.

### list-table-sizes

```lisp
Function: (list-table-sizes &key (schema public) (order-by-size nil) (size t))
```

→ list

Returns a list of lists (table-name, size in 8k pages) of tables in the
current database. Providing a name to the schema parameter will return
just the information for tables in that schema. It defaults to just the
tables in the public schema. Setting schema to nil will return all
tables, indexes etc in the database in descending order of size. This
would include system tables, so there are a lot more than you would
expect. If :size is set to nil, it returns only a flat list of table
names. Setting order-by-size to t will return the result in order of
size instead of by table name.

### list-tables

```lisp
Function: (list-tables &optional strings-p)
```

→ list

Returns a list of the tables in the current database and schema. When
strings-p is T, the names will be given as strings, otherwise as
keywords.

### list-tables-in-schema

```lisp
Function: (list-tables-in-schema &optional (schema-name public) lisp-strings-p)
```

→ list

Returns a list of the tables in the current database and the specified
schema. When strings-p is T,the names will be given as strings,
otherwise as keywords.

### list-tablespaces

```lisp
Function: (list-tablespaces)
```

→ list

Lists the tablespaces in the currently connected database.

### list-triggers

```lisp
Function: (list-triggers &optional table-name)
```

→ list

List distinct trigger names from the information\_schema table.
Table-name can be either quoted or string.

### list-unique-or-primary-constraints

```lisp
Function: (list-unique-or-primary-constraints table-name &optional (strings-p))
```

→ list

List constraints on a table.

### list-views

```lisp
Function: (list-views &optional strings-p)
```

→ list

Returns list of the user defined views in the current database. When
strings-p is T, the names will be returned as strings, otherwise as
keywords.

### make-dao

```lisp
Generic Function: (make-dao type &rest args &key &allow-other-keys)
```

→ dao

Combines make-instance with insert-dao. Return the created dao.

### more-table-info

```lisp
Function: (more-table-info table-name)
```

→ list

Returns more table info than table-description. Table can be either a
string or quoted.

### num-records-in-database

```lisp
Function: (num-records-in-database)
```

→ list

Returns a list of lists with schema, table name and approximate number
of records in the currently connected database.

### numeric

### parse-queries

```lisp
Function: (parse-queries file-content)
```
read SQL queries in given string and split them, returns a list

### prepare

```lisp
Macro: (prepare query &optional (format rows))
```

→ function

Wraps a query into a function that can be used as the interface to a
prepared statement. The given query (either a string or an S-SQL form)
may contain placeholders, which look like \$1, \$2, etc. The resulting
function takes one argument for every placeholder in the query, executes
the prepared query, and returns the result in the format specified.
(Allowed formats are the same as for query.)

For queries that have to be run very often, especially when they are
complex, it may help performance since the server only has to plan them
once. See the [PostgreSQL
manual](http://www.postgresql.org/docs/current/static/sql-prepare.html)
for details.

In some cases, the server will complain about not being able to deduce
the type of the arguments in a statement. In that case you should add
type declarations (either with the PostgreSQL's CAST SQL-conforming
syntax or historical :: syntax, or with S-SQL's :type construct) to help
it out.

Note that it will attempt to automatically reconnect if
database-connection-error, or admin-shutdown. It will reset prepared
statements triggering an invalid-sql-statement-name error. It will
overwrite old prepared statements triggering a
duplicate-prepared-statement error.

### prepared-statement-exists-p

```lisp
Function: (prepared-statement-exists-p name)
```

→ boolean This returns t if the prepared statement exists in the current
postgresql session, otherwise nil.

### query

```lisp
Macro: (query query &rest args/format)
```

→ result

Execute the given query, which can be either a string or an S-SQL form
(list starting with a keyword). If the query contains placeholders (\$1,
\$2, etc) their values can be given as extra arguments. If one of these
arguments is a keyword occurring in the table below, it will not be used
as a query argument, but will determine the format in which the results
are returned instead. Any of the following formats can be used, with the
default being :rows:

```
:none	            | Ignore the result values.                                                                                                                                |
:lists, :rows       | Return a list of lists, each list containing the values for a row.                                                                                     |
:list, :row         | Return a single row as a list.                                                                                                                         |
:alists	            | Return a list of alists which map column names to values, with the names represented as keywords.                                                        |
:alist	            | Return a single row as an alist.                                                                                                                         |
:array-hash         | Return an array of hashtables which map column names to hash table keys                                                                                  |
:str-alists         | Like :alists, but use the original column names.                                                                                                       |
:str-alist	        | Return a single row as an alist, with strings for names.                                                                                                 |
:plists	            | Return a list of plists which map column names to values,with the names represented as keywords.                                                         |
:plist	            | Return a single row as a plist.                                                                                                                          |
:column	            | Return a single column as a list.                                                                                                                        |
:single	            | Return a single value.                                                                                                                                   |
:single!	        | Like :single, but raise an error when the number of selected rows is not equal to 1.                                                                     |
(:dao type)	        | Return a list of DAOs of the given type. The names of the fields returned by the query must match slots in the DAO class the same way as with query-dao. |
(:dao type :single) | Return a single DAO of the given type.                                                                                                                 |
```

If the database returns information about the amount rows that were
affected, such as with updating or deleting queries, this is returned as
a second value.

### query-dao

```lisp
Macro: (query-dao type query &rest args)
```

→ list

Execute the given query (which can be either a string or an S-SQL
expression) and return the result as DAOs of the given type. If the
query contains placeholders (\$1, \$2, etc) their values can be given as
extra arguments. The names of the fields returned by the query must
either match slots in the DAO class, or be bound through
with-column-writers.

### read-queries

```lisp
Function: (read-queries filename)
```
read SQL queries in given file and split them, returns a list

### real


```lisp
NIL
```

### reconnect

```lisp
Generic Function: (reconnect database)
```

Reconnect a disconnected database connection. This is not allowed for
pooled connections ― after they are disconnected they might be in use by
some other process, and should no longer be used.

### register-sql-operators

```lisp
Macro: (register-sql-operators arity &rest names)
```
Define simple operators. Arity is one of :unary (like
'not'), :unary-postfix (the operator comes after the operand),
:n-ary (like '+': the operator falls away when there is only one
operand), :2+-ary (like '=', which is meaningless for one operand),
or :n-or-unary (like '-', where the operator is kept in the unary
case). After the arity follow any number of operators, either just a
keyword, in which case the downcased symbol name is used as the
operator, or a two-element list containing a keyword and a name
string.

### release-savepoint

```lisp
Function: (release-savepoint savepoint)
```

Release the given savepoint.

### reset-prepared-statement

```lisp
Function: (reset-prepared-statement condition)
```

→ restart

If you have received an invalid-prepared-statement error but the
prepared statement is still in the meta slot in the postmodern
connection, this will try to regenerate the prepared statement at the
database connection level and restart the connection.

### rollback-savepoint

```lisp
Function: (rollback-savepoint savepoint)
```

Roll back the given savepoint.

### save-dao

```lisp
Function: (save-dao dao)
```

→ boolean

Tries to insert the given dao using insert-dao. If this raises a unique
key violation error, it tries to update it by using update-dao instead.
Be aware that there is a possible race condition here ― if some other
process deletes the row at just the right moment, the update fails as
well. Returns a boolean telling you whether a new row was inserted.

This function is unsafe to use inside of a transaction ― when a row with
the given keys already exists, the transaction will be aborted. Use
save-dao/transaction instead in such a situation.

See also: upsert-dao.

### save-dao/transaction

```lisp
Function: (save-dao/transaction dao)
```

→ boolean

Acts exactly like save-dao, except that it protects its attempt to
insert the object with a rollback point, so that a failure will not
abort the transaction.

See also: upsert-dao.

### schema-exists-p

```lisp
Function: (schema-exists-p name)
```

→ boolean

Tests the existence of a given schema. Returns T if the schema exists or
NIL otherwise.

### select-dao

```lisp
Macro: (select-dao type &optional (test t) &rest ordering)
```

→ list

Select DAO objects for the rows in the associated table for which the
given test (either an S-SQL expression or a string) holds. When sorting
arguments are given, which can also be S-SQL forms or strings, these are
used to sort the result.

(Note that, if you want to sort, you have to pass the test argument.)

``` {.commonlisp}
(select-dao 'user (:> 'score 10000) 'name)
```

### sequence-exists-p

```lisp
Function: (sequence-exists-p sequence)
```

→ boolean

Tests whether a sequence with the given name exists. The name can be
either a string or a symbol.

### sequence-next

```lisp
Function: (sequence-next sequence)
```

→ integer

Get the next value from a sequence. The sequence identifier can be
either a string or a symbol, in the latter case it will be converted to
a string according to S-SQL rules.

### set-search-path

```lisp
Function: (set-search-path path)
```

Sets the search path to the path. This function is used by with-schema.

### split-fully-qualified-table-name

```lisp
: (split-fully-qualified-table-name)
```

→ list Takes a name of the form database.schema.table or schema.table or
just table and returns a list in the form '(table schema database)

### sql

```lisp
Macro: (sql form)
```
Compile form to a sql expression as far as possible.

### sql-compile

```lisp
Function: (sql-compile form)
```


### sql-error

```lisp
Function: (sql-error control &rest args)
```



```lisp
Condition
```

### sql-escape

```lisp
Generic Function: (sql-escape arg)
```
Get the representation of a Lisp value so that it
can be used in a query.

### sql-escape-string

```lisp
Function: (sql-escape-string string &optional prefix)
```
Escape string data so it can be used in a query.

### table-description

```lisp
Function: (table-description table-name &optional schema-name)
```

→ list

Returns a list of the fields in the named table. Each field is
represented by a list of three elements: the field name, the type, and a
boolean indicating whether the field may be NULL. Optionally,
schema-name can be specified to restrict the result to fields from the
named schema. Without it, all fields in the table are returned,
regardless of their schema.

### table-exists-p

```lisp
Function: (table-exists-p table-name &optional (schema-name nil))
```

→ boolean

Tests whether a table with the given name exists. The name can be either
a string or a symbol. It can also be qualified in the form of
'schema.table or 'database.schema.table

### table-size

```lisp
Function: (table-size table-name)
```

→ list

Return the size of a postgresql table in k or m. Table-name can be
either a string or quoted.

### terminate-backend

```lisp
Function: (terminate-backend pid &optional (database *database*))
```

Less polite way of terminating at the database (as opposed to calling
close-database). Faster than (cancel-backend pid) and more reliable.

### unused-indexes

```lisp
Function: (unused-indexes)
```

→ list

Returns a list of lists showing schema.table, indexname, index\_size and
number of scans. The code was borrowed from:
<https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/>

### update-dao

```lisp
Generic Function: (update-dao dao)
```

→ dao

Update the representation of the given dao in the database to the values
in the object. This is not defined for tables that do not have any
non-primary-key columns. Raises an error when no row matching the dao
exists.

### upsert-dao

```lisp
Generic Function: (upsert-dao dao)
```

→ dao

Like save-dao or save-dao/transaction but using a different method that
doesn't involve a database exception. This is safe to use both in and
outside a transaction, though it's advisable to always do it in a
transaction to prevent a race condition. The way it works is:

If the object contains unbound slots, we call insert-dao directly, thus
the behavior is like save-dao.

Otherwise we try to update a record with the same primary key. If the
PostgreSQL returns a non-zero number of rows updated it treated as the
record is already exists in the database, and we stop here.

If the PostgreSQL returns a zero number of rows updated, it treated as
the record does not exist and we call insert-dao.

The race condition might occur at step 3 if there's no transaction: if
UPDATE returns zero number of rows updated and another thread inserts
the record at that moment, the insertion implied by step 3 will fail.

Note, that triggers and rules may affect the number of inserted or
updated rows returned by PostgreSQL, so zero or non-zero number of
affected rows may not actually indicate the existence of record in the
database.

This method returns two values: the DAO object and a boolean (T if the
object was inserted, NIL if it was updated).

### varchar

### view-exists-p

```lisp
Function: (view-exists-p view)
```

→ boolean

Tests whether a view with the given name exists. The name can be either
a string or a symbol.

### with-column-writers

```lisp
Macro: (with-column-writers (&rest defs) &body body)
```

Provides control over the way get-dao, select-dao, and query-dao read
values from the database. This is not commonly needed, but can be used
to reduce the amount of queries a system makes. writers should be a list
of alternating column names (strings or symbols) and writers, where
writers are either symbols referring to a slot in the objects, or
functions taking two arguments ― an instance and a value ― which can be
used to somehow store the value in the new instance. When any
DAO-fetching function is called in the body, and columns matching the
given names are encountered in the result, the writers are used instead
of the default behaviour (try and store the value in the slot that
matches the column name).

An example of using this is to add some non-column slots to a DAO class,
and use query-dao within a with-column-writers form to pull in extra
information about the objects, and immediately store it in the new
instances.

### with-connection

```lisp
Macro: (with-connection spec &body body)
```

Evaluates the body with \*database\* bound to a connection as specified
by spec, which should be list that connect can be applied to.

### with-logical-transaction

```lisp
Macro: (with-logical-transaction (&optional (name) (isolation-level)) &body body)
```

Executes body within a with-transaction form if no transaction is
currently in progress, otherwise simulates a nested transaction by
executing it within a with-savepoint form. The transaction or savepoint
is bound to name if one is supplied. The isolation-level will set the
isolation-level used by the transaction.

You can specify the following isolation levels in postmodern
transactions:

-   :read-committed-rw (read committed with read and write)
-   :read-committed-ro (read committed with read only)
-   :repeatable-read-rw (repeatable read with read and write)
-   :repeatable-read-ro (repeatable read with read only)
-   :serializable (serializable with reand and write)

Sample usage where "george" is just the name given to the transaction
(not quoted or a string) and ... simply indicates other statements would
be expected here:

``` {.commonlisp}
(with-logical-transaction ()
  (execute (:insert-into 'test-data :set 'value 77))
  ...)

(with-logical-transaction (george)
  (execute (:insert-into 'test-data :set 'value 22))
  ...)

(with-logical-transaction (george :read-committed-rw)
  (execute (:insert-into 'test-data :set 'value 33))
  ...)

(with-logical-transaction (:serializable)
  (execute (:insert-into 'test-data :set 'value 44))
  ...)
```

### with-savepoint

```lisp
Macro: (with-savepoint name &body body)
```

Can only be used within a transaction. Establishes a savepoint with the
given name at the start of body, and binds the same name to a handle for
that savepoint. At the end of body, the savepoint is released, unless a
condition is thrown, in which case it is rolled back.

### with-schema

```lisp
Macro: (with-schema (schema &key (strict) (if-not-exist) (drop-after)) &body form)
```

Sets the current schema to namespace and executes the body. Before
executing body the PostgreSQL's session variable search\_path is set to
the given namespace. After executing body the search\_path variable is
restored to the original value. If the keyword :strict is set to T then
the namespace is only the scheme on the search path upon the body
execution. Otherwise the namespace is just first schema on the search
path upon the the body execution. If :if-not-exist is NIL, an error is
signaled. If :drop-after is T the namespace is dropped from the database
after the body execution.

### with-transaction

```lisp
Macro: (with-transaction (&optional name isolation-level) &body body)
```

Execute the given body within a database transaction, committing it when
the body exits normally, and aborting otherwise. An optional name and/or
isolation-level can be given to the transaction. The name can be used to
force a commit or abort before the body unwinds. The isolation-level
will set the isolation-level used by the transaction.

You can specify the following isolation levels in postmodern
transactions:

-   :read-committed-rw (read committed with read and write)
-   :read-committed-ro (read committed with read only)
-   :repeatable-read-rw (repeatable read with read and write)
-   :repeatable-read-ro (repeatable read with read only)
-   :serializable (serializable with reand and write)

Sample usage where "george" is just the name given to the transaction
(not quoted or a string) and ... simply indicates other statements would
be expected here:

``` {.commonlisp}
(with-transaction ()
  (execute (:insert-into 'test-data :set 'value 77))
  ...)

(with-transaction (george)
  (execute (:insert-into 'test-data :set 'value 22))
  ...)

(with-transaction (george :read-committed-rw)
  (execute (:insert-into 'test-data :set 'value 33))
  (query (:select '* :from 'test-data))
  ...)

(with-transaction (:serializable)
  (execute (:insert-into 'test-data :set 'value 44))
  ...)
```

Further discussion of transactions and isolation levels can found
[here](isolation-notes.html).
