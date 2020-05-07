# postmodern

Version: 1.30
<br/>
Repository: [marijnh/Postmodern - Github](https://github.com/marijnh/Postmodern)

*This page was possible due to the excellent [official documentation](https://edicl.github.io/hunchentoot/) as well as the page on [Web Development on The Common Lisp Cookbook](http://lispcookbook.github.io/cl-cookbook/web.html).*

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

[TODO] Someone more familiar with postgres or databases should review this section.

Follow the [installation instructions](https://www.postgresql.org/download/) to install Postgres.
Once done, you should have access to the `postgres`, `pg_ctl`, and optionally the `psql` commands on your command line / terminal.

Once done, [this page](https://www.postgresql.org/docs/10/server-start.html) elaborates
the process of starting the database server and any issues that may arise. (You can select the postgresql version from top of the page.)

- Initialize the directory: `pg_ctl init -D postmodern # see \`pg_ctl --help`\ from the options`.
- Optionally, change `port` and `unix_socket_directories` from `postmodern/postgresql.conf`.
- `pg_ctl start -D postmodern` to start the server.

You should get a `server started` message; if not, the link above should help in debugging.
Proceed to the next section once you successfully start the server.

[This page](https://www.postgresql.org/docs/10/runtime-config-connection.html) elaborates on the configuration settings.

In addition, you can list the databases by using
`psql -p`*`PORT`*`-h`*`unix_socket_directories`*`-l`,
replacing the italicized arguments appropriately. 

### Connecting to the Postgres server

We firstly connect to the default existing database. Create a new database for our
purposes, and then disconnect and reconnect to this database.

We also assume the server is started at 8080 and username is `"username"`.

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

[TODO] Distinguish why "exactly" connect and connect-toplevel are different? What goes on under the hood? Also, what role does `:pooled-p` play?

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

[TODO] The behaviour of `(get-dao 'points :x 1)`: https://github.com/marijnh/Postmodern/issues/230

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

[TODO] Groaking migrations needed for ELI5-cation. 

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

**Installation**

[Linux](https://www.postgresql.org/download/linux/ubuntu/)

## CONFIGURATION VARIABLES

### \*allow-overwriting-prepared-statements\*

```lisp
Variable
```

When set to t, ensured-prepared will overwrite prepared statements having
the same name if the query statement itself in the postmodern meta connection
is different than the query statement provided to ensure-prepared.

### \*current-logical-transaction\*

### \*database\*

```lisp
Variable
```

Special holding the current database. Most functions and macros
operating on a database assume this contains a connected database.

### \*default-use-ssl\*

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

### \*isolation-level\*

### \*max-pool-size\*

```lisp
Variable
```

The maximum amount of connection that will be kept in a single
pool, or NIL for no maximum.

### \*table-name\*

```lisp
Variable
```

Used inside [deftable](#deftable) to find the name of the table being defined.

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
Used inside a [deftable](#deftable) form. Define this table using the
corresponding DAO class' slots.

### !foreign

```lisp
Function: (!foreign target fields &rest target-fields/on-delete/on-update/deferrable/initially-deferred)
```
Used inside a [deftable](#deftable) form. Define a foreign key on this table.
Pass a table the index refers to, a list of fields or single field in
*this* table, and, if the fields have different names in the table
referred to, another field or list of fields for the target table, or
:primary-key to indicate that the other table's primary key should be
referenced.

### !index

```lisp
Function: (!index &rest fields)
```
Used inside a [deftable](#deftable) form. Define an index on the defined table.

### !unique

```lisp
Function: (!unique target-fields &key deferrable initially-deferred)
```


### !unique-index

```lisp
Function: (!unique-index &rest fields)
```
Used inside a [deftable](#deftable) form. Define a unique index on the defined table.

### abort-hooks

```lisp
Generic Function: (abort-hooks object)
```

```lisp
Generic Function: (setf (abort-hooks object) ...)
```


### abort-transaction

```lisp
Function: (abort-transaction transaction)
```
Immediately abort an open transaction.

### bigint

### bloat-measurement

```lisp
Function: (bloat-measurement)
```
Bloat measurement of unvacuumed dead tuples. Borrowed from: https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/

### bytea

### cache-hit-ratio

```lisp
Function: (cache-hit-ratio)
```
The cache hit ratio shows data on serving the data from memory compared to how often you have to go to disk.
This function returns a list of heapblocks read from disk, heapblocks hit from memory and the ratio of
heapblocks hit from memory / total heapblocks hit.
Borrowed from: https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/

### call-with-connection

```lisp
Function: (call-with-connection spec thunk)
```
Binds \*database\* to a new connection, as specified by the spec
argument, which should be a list of arguments that can be passed to
connect, and runs the function given as a second argument with that
database.

### cancel-backend

```lisp
Function: (cancel-backend pid &optional (database *database*))
```
Polite way of terminating a query at the database (as opposed to calling close-database).
Slower than (terminate-backend pid) and does not always work.

### change-toplevel-database

```lisp
Function: (change-toplevel-database new-database user password host)
```
Just changes the database assuming you are using a toplevel connection.
Recommended only for development work.

### check-query-performance

```lisp
Function: (check-query-performance &optional (ob nil) (num-calls 100) (limit 20))
```
This function requires that postgresql extension pg_stat_statements must be loaded via shared_preload_libraries.
It is borrowed from https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/.
Optional parameters `ob` allow order-by to be 'calls', 'total-time', 'rows-per' or 'time-per', defaulting to time-per.
num-calls to require that the number of calls exceeds a certain threshold, and limit to limit the number of rows returned.
It returns a list of lists, each row containing the query, number of calls, total_time, total_time/calls, stddev_time, rows,
rows/calls and the cache hit percentage.

### clear-connection-pool

```lisp
Function: (clear-connection-pool)
```
Disconnect and remove all connections in the connection pool.

### coalesce

```lisp
Function: (coalesce &rest args)
```
Returns t if any argument is not nil or :null.

### column-exists-p

```lisp
Function: (column-exists-p table-name column-name)
```
Determine if a particular column exists. Table name and column-name can be either strings or symbols.

### commit-hooks

```lisp
Generic Function: (commit-hooks object)
```

```lisp
Generic Function: (setf (commit-hooks object) ...)
```


### commit-transaction

```lisp
Function: (commit-transaction transaction)
```
Immediately commit an open transaction.

### connect

```lisp
Function: (connect database-name user password host
            &key (port 5432) pooled-p (use-ssl *default-use-ssl*) (service "postgres"))
```
Create and return a database connection.

### connect-toplevel

```lisp
Function: (connect-toplevel database-name user password host
            &key (port 5432) (use-ssl *default-use-ssl*))
```
Set \*database\* to a new connection. Use this if you only need one
connection, or if you want a connection for debugging from the REPL.

### connected-p

```lisp
Function: (connected-p database)
```
Test whether a database connection is still connected.

### create-all-tables

```lisp
Function: (create-all-tables)
```
Create all defined tables.

### create-index

```lisp
Function: (create-index name &key unique if-not-exists concurrently on using fields)
```
Create an index. Slightly less sophisticated than the query version because
it does not have a where clause capability.

### create-package-tables

```lisp
Function: (create-package-tables package)
```
Create all tables whose identifying symbol is interned in the given
package.

### create-schema

```lisp
Function: (create-schema schema)
```
Creating a non existing schema.
   If the schema exists an error is raised.

### create-sequence

```lisp
Function: (create-sequence name &key temp if-not-exists increment min-value max-value start cache)
```
Create a sequence. Available additional key parameters are
:temp :if-not-exists :increment :min-value :max-value :start and :cache. See
https://www.postgresql.org/docs/current/static/sql-createsequence.html for details on usage.

### create-table

```lisp
Function: (create-table name)
```
Create a defined table.

### current-database

```lisp
Function: (current-database)
```
Returns the string name of the current database.

### dao-class


```lisp
Class
```

DAO-class metaclass
allows you to define classes for your database-access objects as regular
CLOS classes.

- To specify that a slot refers to a column, give it a
`:col-type` option containing an S-SQL type expression (useful if you want
to be able to derive a table definition from the class definition),
- or simply a `:column` option with value T.
- Such slots can also take a `:col-default` option, used to provide a database-side default value as an S-SQL expression.
- You can use the `:col-name` initarg (whose unevaluated
value will be passed to to-sql-name) to specify the slot's column's
name.

DAO class definitions support two extra class options:

- `:table-name` to give the name of the table that the class refers to (defaults to the
class name),
- and `:keys` to provide a set of primary keys for the table.

When no primary keys are defined, operations such as [update-dao](#update-dao) and
[get-dao](#get-dao) will not work.

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
Return a boolean indicating whether the given dao
  exists in the database.

### dao-keys

```lisp
Generic Function: (dao-keys class)
```
Returns list of slot names that are the primary key of DAO
class. This is likely interesting if you have primary keys which are composed
of more than one slot. Pay careful attention to situations where the primary
key not only has more than one column, but they are actually in a different
order than they are in the database table itself. You can check this with the
find-primary-key-info function.

### dao-table-definition

```lisp
Function: (dao-table-definition table)
```
Generate the appropriate CREATE `table` query for this class.

### dao-table-name

```lisp
Function: (dao-table-name class)
```


### database-connection


```lisp
Class
```

Representation of a database connection. Contains
login information in order to be able to automatically re-establish a
connection when it is somehow closed.

<u>**Direct Slots**</u>

**cl-postgres::host**
```lisp
Initargs: :HOST
Readers: CL-POSTGRES::CONNECTION-HOST
```
**cl-postgres::port**
```lisp
Initargs: :PORT
Readers: CL-POSTGRES::CONNECTION-PORT
```
**cl-postgres::database**
```lisp
Initargs: :DB
Readers: CL-POSTGRES::CONNECTION-DB
```
**cl-postgres::user**
```lisp
Initargs: :USER
Readers: CL-POSTGRES::CONNECTION-USER
```
**cl-postgres::password**
```lisp
Initargs: :PASSWORD
Readers: CL-POSTGRES::CONNECTION-PASSWORD
```
**cl-postgres::use-ssl**
```lisp
Initargs: :SSL
Readers: CL-POSTGRES::CONNECTION-USE-SSL
```
**cl-postgres::service**
```lisp
Initargs: :SERVICE
Readers: CL-POSTGRES::CONNECTION-SERVICE
```
**cl-postgres::socket**
```lisp
Initargs: :SOCKET
Readers: CL-POSTGRES::CONNECTION-SOCKET
```
**cl-postgres::meta**
```lisp
```
**cl-postgres::available**
```lisp
```
**cl-postgres::parameters**
```lisp
```
**cl-postgres::timestamp-format**
```lisp
```
### database-connection-error


```lisp
Condition
```

Conditions of this type are signalled when an error
occurs that breaks the connection socket. They offer a :reconnect
restart.

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
Determine if a particular database exists. 

### database-size

```lisp
Function: (database-size &optional (name nil))
```
Given the name of a database, will return the name, a pretty-print string of
the size of the database and the size in bytes. If a database name is not provided,
it will return the result for the currently connected database.

### database-version

```lisp
Function: (database-version)
```
Returns the version of the current postgresql database.

### db-null

### define-dao-finalization

```lisp
Macro: (define-dao-finalization ((dao-name class) &rest keyword-args) &body body)
```


### defprepared

```lisp
Macro: (defprepared name query &optional (format rows))
```
Like prepare, but gives the function a name instead of returning
it. The name should not be a string but may be quoted.

### defprepared-with-names

```lisp
Macro: (defprepared-with-names name (&rest args) (query &rest query-args) &optional (format
                                                                                     rows))
```
Like defprepared, but with lambda list for statement arguments.

### deftable

```lisp
Macro: (deftable name &body definitions)
```
Define a table. name can be either a symbol or a (symbol string)
list. In the first case, the table name is derived from the symbol by
S-SQL's rules, in the second case, the name is given explicitly. The
body of definitions can contain anything that evaluates to a string,
as well as S-SQL expressions. In this body, the variables *table-name*
and *table-symbol* are bound to the relevant values.

### delete-dao

```lisp
Generic Function: (delete-dao dao)
```
Delete the given dao from the database.

### describe-constraint

```lisp
Function: (describe-constraint table-name constraint-name)
```
Return a list of alists of the descriptions a particular constraint given
the table-name and the  constraint name using the information_schema
table.

### describe-foreign-key-constraints

```lisp
Function: (describe-foreign-key-constraints)
```
Generates a list of lists of information on the foreign key constraints

### describe-views

```lisp
Function: (describe-views &optional (schema public))
```
Describe the current views in the specified schema. Takes an optional schema
name but defaults to public schema.

### disconnect

```lisp
Generic Function: (disconnect database)
```
Close a database connection. Returns it to a pool
if it is a pooled connection.

### disconnect-toplevel

```lisp
Function: (disconnect-toplevel)
```
Disconnect \*database\*.

### do-query-dao

```lisp
Macro: (do-query-dao ((type type-var) query) &body body)
```
Like query-dao, but rather than returning a list of results,
executes `body` once for each result, with `type-var` bound to the DAO
representing that result.

### do-select-dao

```lisp
Macro: (do-select-dao ((type type-var) &optional (test) &rest ordering) &body body)
```
Like select-dao, but rather than returning a list of results,
executes `body` once for each result, with `type-var` bound to the DAO
representing that result.

### doquery

```lisp
Macro: (doquery query (&rest names) &body body)
```
Iterate over the rows in the result of a query, binding the given
names to the results and executing body for every row. Query can be a
string, an s-sql query, or a list starting with one of those, followed
by the arguments to parameterize the query with.

### double-precision

### drop-index

```lisp
Function: (drop-index name &key concurrently if-exists cascade)
```
Drop an index. Available keys are :concurrently, :if-exists, and :cascade.

### drop-prepared-statement

```lisp
Function: (drop-prepared-statement name &key (location both) (database
                                                              *database*) (remove-function
                                                                           t))
```
Prepared statements are stored both in the meta slot in the postmodern
connection and in postgresql session information. In the case of prepared
statements generated with defprepared, there is also a lisp function with
the same name.

If you know the prepared statement name, you can delete the prepared statement
from both locations (the default behavior), just from postmodern by passing
:postmodern to the location key parameter or just from postgresql by passing
:postgresql to the location key parameter.

If you pass the name 'All' as the statement name, it will
delete all prepared statements.

The default behavior is to also remove any lisp function of the same name.
This behavior is controlled by the remove-function key parameter.

### drop-schema

```lisp
Function: (drop-schema schema &key (if-exists nil) (cascade nil))
```
Drops an existing database schema 'schema'
A notice instead of an error is raised with the is-exists parameter.

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
Executes body within a with-transaction form if and only if no
transaction is already in progress.

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
Execute a query, ignore the results.

### execute-file

```lisp
Function: (execute-file pathname &optional (print nil))
```
Executes all queries in the provided SQL file. If print is set to t,
 format will print the count of query and the query.

### find-postgresql-prepared-statement

```lisp
Function: (find-postgresql-prepared-statement name)
```
Returns the specified named prepared statement (if any) that postgresql
has for this session.

### find-postmodern-prepared-statement

```lisp
Function: (find-postmodern-prepared-statement name)
```
Returns the specified named prepared statement (if any) that postmodern has put in
the meta slot in the connection.

### find-primary-key-info

```lisp
Function: (find-primary-key-info table &optional (just-key nil))
```
Returns a list of sublists where the sublist contains two strings.
If a table primary key consists of only one column, such as 'id' there
will be a single sublist where the first string is the name of the column
and the second string is the string name for the datatype for that column.
If the primary key for the table consists of more than one column, there
will be a sublist for each column subpart of the key. The sublists will
be in the order they are used in the key, not in the order they appear
in the table. If just-key is set to t, the list being returned will
contain just the column names in the primary key as string names
with no sublists. If the table is not in the public schema, provide
the fully qualified table name e.g. schema-name.table-name.

### get-dao

```lisp
Generic Function: (get-dao type &rest args)
```
Get the object corresponding to the given primary
  key, or return nil if it does not exist.

### get-pid

```lisp
Function: (get-pid)
```
Get the process id used by postgresql for this connection.

### get-pid-from-postmodern

```lisp
Function: (get-pid-from-postmodern)
```
Get the process id used by postgresql for this connection,
but get it from the postmodern connection parameters.

### get-search-path

```lisp
Function: (get-search-path)
```
Returns the default schema search path for the current session.

### index-exists-p

```lisp
Function: (index-exists-p index-name)
```
Check whether a index exists. Takes either a string or a symbol for
the index name.

### insert-dao

```lisp
Generic Function: (insert-dao dao)
```
Insert the given object into the database.

### list-all-constraints

```lisp
Function: (list-all-constraints table-name &optional (strings-p))
```
Uses information_schema to list all the constraints in a table. Table-name
can be either a string or quoted. Turns constraints into keywords if strings-p is not true.

### list-available-extensions

```lisp
Function: (list-available-extensions)
```
Returns available postgresql extensions per pg_available_extensions

### list-available-types

```lisp
Function: (list-available-types)
```
List the available types in this postgresql version.

### list-columns

```lisp
Function: (list-columns table-name)
```
Returns a list of strings of just the column names in a table.
Pulls info from the postmodern table-description function
rather than directly.

### list-columns-with-types

```lisp
Function: (list-columns-with-types table-name)
```
Return a list of (name type) lists for the fields of a table. Goes directly to the pg-catalog tables.

### list-connections

```lisp
Function: (list-connections)
```
Returns info from pg_stat_activity on open connections

### list-database-functions

```lisp
Function: (list-database-functions)
```
Returns a list of the functions in the database from the information_schema.

### list-database-users

```lisp
Function: (list-database-users)
```
List database users.

### list-databases

```lisp
Function: (list-databases &key (order-by-size nil) (size t))
```
Returns a list of lists where each sub-list contains the name of the
database, a pretty-print string of the size of that database and the size in bytes.
The default order is by database name. Pass t as a parameter to :order-by-size for order by size.
Setting size to nil will return just the database names in a single list
ordered by name. This function excludes the template databases.

### list-detailed-triggers

```lisp
Function: (list-detailed-triggers)
```
List detailed information on the triggers from the information_schema table.

### list-foreign-keys

```lisp
Function: (list-foreign-keys table schema)
```
Returns a list of sublists of foreign key info in the form of
   '((constraint-name local-table local-table-column
     foreign-table-name foreign-column-name))

### list-index-definitions

```lisp
Function: (list-index-definitions table-name)
```
Returns a list of the definitions used to create the current indexes for the table.

### list-indexed-column-and-attributes

```lisp
Function: (list-indexed-column-and-attributes table-name)
```
List the indexed columns and their attributes in a table. Includes primary key.

### list-indices

```lisp
Function: (list-indices &optional strings-p)
```
Return a list of the indexs in a database. Turn them into keywords if strings-p is not true.

### list-installed-extensions

```lisp
Function: (list-installed-extensions)
```
Returns postgresql extensions actually installed in the database per pg_available_extensions

### list-postmodern-prepared-statements

```lisp
Function: (list-postmodern-prepared-statements &optional (names-only nil))
```
List the prepared statements that postmodern has put in the meta slot in
the connection. It will return a list of alists of form:
  ((:NAME . "SNY24")
  (:STATEMENT . "(SELECT name, salary FROM employee WHERE (city = $1))")
  (:PREPARE-TIME . #<TIMESTAMP 25-11-2018T15:36:43,385>)
  (:PARAMETER-TYPES . "{text}") (:FROM-SQL).

If the names-only parameter is set to t, it will only return a list of
the names of the prepared statements.

### list-prepared-statements

```lisp
Function: (list-prepared-statements &optional (names-only nil))
```
Syntactic sugar. A query that lists the prepared statements
in the session in which the function is run. If the optional
names-only parameter is set to t, it will only return a list
of the names of the prepared statements.

### list-roles

```lisp
Function: (list-roles &optional (lt nil))
```
Returns a list of alists of rolenames, role attributes and membership in roles.
See https://www.postgresql.org/docs/current/role-membership.html for an explanation.
The optional parameter can be used to set the return list types to :alists or :plists.

### list-schemas

```lisp
Function: (list-schemas)
```
List schemas in the current database, excluding the pg_* system schemas.

### list-schemata

```lisp
Function: (list-schemata)
```
List all existing user defined schemata.

  Note: The query uses the portable information_schema relations instead of pg_tables relations
  SELECT schema_name FROM information_schema.schemata where schema_name !~ '(pg_*)|information_schema' ORDER BY schema_name ;

### list-sequences

```lisp
Function: (list-sequences &optional strings-p)
```
Return a list of the sequences in a database. Turn them into
keywords if strings-p is not true.

### list-table-indices

```lisp
Function: (list-table-indices table-name &optional strings-p)
```
List the index names and the related columns in a single table. Each index will be in a separate sublist.

### list-table-sizes

```lisp
Function: (list-table-sizes &key (schema public) (order-by-size nil) (size t))
```
Returns a list of lists (table-name, size in 8k pages) of tables in the current database.
Providing a name to the schema parameter will return just the information for tables in that schema.
It defaults to just the tables in the public schema. Setting schema to nil will return all tables, indexes etc
in the database in descending order of size. This would include system tables, so there
are a lot more than you would expect. If :size is set to nil, it returns only a flat list of table names.
Setting order-by-size to t will return the result in order of size instead of by table name.

### list-tables

```lisp
Function: (list-tables &optional strings-p)
```
Return a list of the tables in a database. Turn them into keywords
if strings-p is not true.

### list-tables-in-schema

```lisp
Function: (list-tables-in-schema &optional (schema-name public) lisp-strings-p)
```
Returns a list of tables in a particular schema, defaulting to public.

### list-tablespaces

```lisp
Function: (list-tablespaces)
```
Lists the tablespaces in the currently connected database.

### list-triggers

```lisp
Function: (list-triggers &optional table-name)
```
List distinct trigger names from the information_schema table. Table-name can be either quoted or string.

### list-unique-or-primary-constraints

```lisp
Function: (list-unique-or-primary-constraints table-name &optional (strings-p))
```
List constraints on a table. Table-name
can be either a string or quoted. Turns constraints into keywords if strings-p is not true.

### list-views

```lisp
Function: (list-views &optional strings-p)
```
Return a list of the views in a database. Turn them into keywords
if strings-p is not true.

### make-dao

```lisp
Generic Function: (make-dao type &rest args &key &allow-other-keys)
```
Make the instance of the given class and insert it into the database

### more-table-info

```lisp
Function: (more-table-info table-name)
```
Returns more table info than table-description. Table can be either a string or quoted.
Specifically returns ordinal-position, column-name, data-type, character-maximum-length,
modifier, whether it is not-null and the default value. 

### num-records-in-database

```lisp
Function: (num-records-in-database)
```
Returns a list of lists with schema, table name and approximate number of records
in the currently connected database.

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
Wraps a query into a function that will prepare it once for a
connection, and then execute it with the given parameters. The query
should contain a placeholder ($1, $2, etc) for every parameter.

### prepared-statement-exists-p

```lisp
Function: (prepared-statement-exists-p name)
```
Returns t if the prepared statement exists in the current postgresql
session, otherwise nil.

### query

```lisp
Macro: (query query &rest args/format)
```
Execute the given query, which can be either a string or an S-SQL form
(list starting with a keyword). If the query contains placeholders ($1, $2, etc)
their values can be given as extra arguments. If one of these arguments
is a keyword occurring in the table below, it will not be used as a query
argument, but will determine the format in which the results are returned
instead. Any of the following formats can be used, with the default being `:rows`:

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

If the database returns information about the amount rows that were affected,
such as with updating or deleting queries, this is returned as a second value.

### query-dao

```lisp
Macro: (query-dao type query &rest args)
```
Execute a query and return the result as daos of the given type.
The fields returned by the query must match the slots of the dao, both
by type and by name.

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
Reconnect a database connection.

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
Immediately release a savepoint, commiting its results.

### reset-prepared-statement

```lisp
Function: (reset-prepared-statement condition)
```
If you have received an invalid-prepared-statement error or a prepared-statement
already exists error but the prepared statement is still in the meta slot in
the postmodern connection, try to regenerate the prepared statement at the
database connection level and restart the connection.

### rollback-savepoint

```lisp
Function: (rollback-savepoint savepoint)
```
Immediately roll back a savepoint, aborting it results.

### save-dao

```lisp
Function: (save-dao dao)
```
Try to insert the content of a `dao`. If this leads to a unique key
violation, update it instead.

### save-dao/transaction

```lisp
Function: (save-dao/transaction dao)
```


### schema-exists-p

```lisp
Function: (schema-exists-p name)
```
Predicate for schema existence. More consistent with naming scheme for other functions.

### select-dao

```lisp
Macro: (select-dao type &optional (test t) &rest ordering)
```
Select daos for the rows in its table for which the given test
holds, order them by the given criteria.

### sequence-exists-p

```lisp
Function: (sequence-exists-p sequence)
```
Check whether a sequence exists. Takes either a string or a symbol
for the sequence name.

### sequence-next

```lisp
Function: (sequence-next sequence)
```
Shortcut for getting the next value from a sequence.

### set-search-path

```lisp
Function: (set-search-path path)
```
This changes the postgresql runtime parameter controlling what order
schemas are searched. You can always use fully qualified names [schema.table].
By default, this function only changes the search path for the current session.

### smallint

### split-fully-qualified-tablename

```lisp
Function: (split-fully-qualified-tablename name)
```
Take a tablename of the form database.schema.table or schema.table
and return the tablename and the schema name. The name can be a symbol
or a string. Returns a list of form '(table schema database

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
Return a list of (name type null-allowed) lists for the fields of a
table.

### table-exists-p

```lisp
Function: (table-exists-p table-name &optional (schema-name nil))
```
Check whether a table exists in a particular schema. Defaults to the search path.
Takes either a string or a symbol for the table name. The table-name can be fully
qualified in the form of schema.table-name or database.schema.table-name. If
the schema is specified either in a qualified table-name or in the optional
schema-name parameter, we look directly to the information schema tables. Otherwise
we use the search path which can be controlled by being within a with-schema form.

### table-size

```lisp
Function: (table-size table-name)
```
Return the size of a postgresql table in k or m. Table-name can be either a string or quoted.

### terminate-backend

```lisp
Function: (terminate-backend pid &optional (database *database*))
```
Less polite way of terminating at the database (as opposed to calling close-database).
Faster than (cancel-backend pid) and more reliable.

### text

```lisp
Generic Function: (text condition)
```


### unused-indexes

```lisp
Function: (unused-indexes)
```
Returns a list of lists showing schema.table, indexname, index_size and number of scans. The code was borrowed from: https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/

### update-dao

```lisp
Generic Function: (update-dao dao)
```
Update the object's representation in the database
  with the values in the given instance.

### upsert-dao

```lisp
Generic Function: (upsert-dao dao)
```
Update or insert the given dao.  If its primary key
  is already in the database and all slots are bound, an update will
  occur.  Otherwise it tries to insert it.

### varchar

### view-exists-p

```lisp
Function: (view-exists-p view)
```
Check whether a view exists. Takes either a string or a symbol for
the view name.

### with-column-writers

```lisp
Macro: (with-column-writers (&rest defs) &body body)
```


### with-connection

```lisp
Macro: (with-connection spec &body body)
```
Locally establish a database connection, and bind \*database\* to it.

### with-logical-transaction

```lisp
Macro: (with-logical-transaction (&optional (name) (isolation-level)) &body body)
```
Executes the body within a with-transaction (if no transaction is
already in progress) or a with-savepoint (if one is), binding the
transaction or savepoint to `name` (if supplied)

### with-savepoint

```lisp
Macro: (with-savepoint name &body body)
```
Execute the body within a savepoint, releasing savepoint when the
body exits normally, and rolling back otherwise. `name` is both the
variable that can be used to release or rolled back before the body
unwinds, and the SQL name of the savepoint.

### with-schema

```lisp
Macro: (with-schema (schema &key (strict) (if-not-exist) (drop-after)) &body form)
```
A macro to set the schema search path of the postgresql
   database to include as first entry a specified schema.

   calling with strict 't only the specified schema is set as current
   search path. All other schema are then not searched any more.

   calling with if-not-exist set to :create the schema is created if
   this schema did not exist.

   calling with drop-after set to 't the schema is removed after the
   execution of the body form.

   example :
     (with-schema (:schema-name :strict nil :drop-after nil :if-not-exist :error)
            (foo 1)
            (foo 2))

### with-transaction

```lisp
Macro: (with-transaction (&optional name isolation-level) &body body)
```
Execute the body within a database transaction, committing when the
body exits normally, and aborting otherwise. An optional name and/or
isolation-level can be given to the transaction. The name can be used to
force a commit or abort before the body unwinds. The isolation-level
will set the isolation-level used by the transaction.


