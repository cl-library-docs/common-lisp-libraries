# quicklisp - Library Manager

Version: 2020-01-04
<br/>
Licence: BSD-style
<br/>
Repository: [quicklisp/quicklisp-client - Github](https://github.com/quicklisp/quicklisp-client)

*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/digikar99/common-lisp.readthedocs/issues).*


## GETTING STARTED

### Installation


**Download**

Do the equivalent steps on Windows and Mac OS:

```sh
curl -O https://beta.quicklisp.org/quicklisp.lisp     # download quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc # download the PGP signature file
gpg --verify quicklisp.lisp.asc quicklisp.lisp        # verify it against the signing key
```

As of 07 June 2020, Quicklisp release signing key has a fingerprint of `D7A3 489D DEFE 32B7 D0E7 CC61 3079 65AB 028B 5FF7`, an id of `028B5FF7`, and an email of `release@quicklisp.org`. See [here](https://www.quicklisp.org/beta/#installation) for the more recent details.

**Installation**

Load quicklisp.lisp: `sbcl --load quicklisp.lisp # or an equivalent command for your implementation` (optionally, you may want to `rlwrap sbcl --load quicklisp.lisp` for a better
repl experience):

```lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file) ; to automatically load quicklisp at implementation startup
;;; Note that ql acts as a nickname for the quicklisp-client package.
```

`quicklisp-quickstart:install` also takes a couple keyword arguments (see `(describe 'quicklisp-quickstart:install)` - but unless you know what
you are doing, it is recommended to leave them as they are.

### Loading after installation

`(ql:add-to-init-file)` adds instructions to your implementations init-file (`.sbclrc` or
equivalent) to load quicklisp on implementation startup.

However, some runtime options of the implementation may disable loading the init-file -
`sbcl --script` for instance. For these cases, you might want to `alias` (or equivalent)
`alias sbcl-ql='rlwrap sbcl --noinform --load $HOME/quicklisp/setup.lisp'` in your `.bashrc`
(or equivalent).

### A few useful functions and variables

- [quickload](#quickload): `(ql:quickload '("alexandria" "bordeaux-threads"))` or `(ql:quickload "alexandria")`.
- [system-apropos](#system-apropos): `(ql:system-apropos "postgres")`
- [where-is-system](#where-is-system)
- [\*local-project-directories\*](#local-project-directories): you may either want to have this variable modified from implementation init-file, or symlink `~/quicklisp/local-projects` to wherever your projects directory is. This is the location - as the name suggests - where quicklisp searches for local projects.
- [uninstall](#uninstall)
- [update-client](#update-client)

You may also need to run `(asdf:clear-configuration)` to have [ASDF](https://common-lisp.net/project/asdf/asdf.html) reprocess its configuration files while loading a newer library. There is also the [ql:register-local-projects](#register-local-projects) that can help with the same.

### A note on quicklisp dists

Besides being an ASDF system, quicklisp is also the name of a [dist](#dist). This distribution
is released about once per month, with the guarantee that all systems in the distribution build together - note that this does not still imply that they *work* together. For these cases,
you may want to [go back in dist time](http://blog.quicklisp.org/2011/08/going-back-in-dist-time.html). You might, additionally, want to learn about [qlot](https://github.com/fukamachi/qlot) that helps with version pinning - however, given the stability of Common Lisp, that translates to the stability for its (old but gold) libraries, it may be an overkill. But, YMMV.

Also worth a read is [getting libraries into quicklisp](http://blog.quicklisp.org/2015/01/getting-library-into-quicklisp.html). (Hope you took note of the blog!)

The once-per-month release nature of quicklisp can be an issue if you need to work with
bleeding edge packages - for these cases, there exists [Ultralisp](https://ultralisp.org/) - which updates every 5 minutes! This is yet another distribution. Some useful functions for working with distributions include:

- [install-dist](#install-dist)
- [uninstall-dist](#uninstall-dist)
- [update-dist](#update-dist): `(ql:update-dist "quicklisp")`
- [find-dist](#find-dist)
- [enabled-dists](#enabled-dists)
- [enable](#enable)
- [disable](#disable)

### More reading

More reading about quicklisp is available on its [official page](https://www.quicklisp.org/beta/index.html).


## quicklisp-client: API REFERENCE

Nickname: ql
<br/>

The Quicklisp client package, intended for end-user Quicklisp
commands and configuration parameters.


### \*local-project-directories\*

```lisp
Variable
```

The default local projects directory.


### \*quickload-prompt\*

```lisp
Variable
```

When NIL, quickload systems without prompting for enter to
continue, otherwise proceed directly without user intervention.


### \*quickload-verbose\*

```lisp
Variable
```

When NIL, show terse output when quickloading a system. Otherwise,
show normal compile and load output.


### add-to-init-file

```lisp
Function: (add-to-init-file &optional implementation-or-file)
```

Add forms to the Lisp implementation's init file that will load
quicklisp at CL startup.

### available-client-versions

```lisp
Function: (available-client-versions)
```



### available-dist-versions

```lisp
Function: (available-dist-versions name)
```



### bundle-systems

```lisp
Function: (bundle-systems system-names &key include-local-projects to
                          (overwrite t))
```

In the directory `to`, construct a self-contained bundle of libraries
based on `system-names`. For each system named, and its recursive
required systems, unpack its release archive in TO/software/, and
write a system index, compatible with the output of
QL:[write-asdf-manifest-file](#write-asdf-manifest-file), to TO/system-index.txt. Write a loader
script to TO/bundle.lisp that, when loaded via CL:LOAD, configures
ASDF to load systems from the bundle before any other system.

`system-names` must name systems provided directly by Quicklisp.

If `include-local-projects` is true, each directory in
QL:*LOCAL-PROJECT-DIRECTORIES* is copied into the bundle and loaded
before any of the other bundled systems.

### client-url

```lisp
Function: (client-url)
```

Return an URL suitable for passing as the :URL argument to
[install-client](#install-client) for the current local client installation.

### client-version

```lisp
Function: (client-version)
```

Return the version for the current local client installation. May
or may not be suitable for passing as the :VERSION argument to
[install-client](#install-client), depending on if it's a standard Quicklisp-provided
client.

### config-value

```lisp
Function: (config-value path)
```



### dist-url

```lisp
Function: (dist-url name)
```



### dist-version

```lisp
Function: (dist-version name)
```



### help

```lisp
Function: (help)
```



### install-client

```lisp
Function: (install-client &key url version)
```



### list-local-projects

```lisp
Function: (list-local-projects)
```

Return a list of pathnames to local project system files.

### list-local-systems

```lisp
Function: (list-local-systems)
```

Return a list of local project system names.

### local-projects-searcher

```lisp
Function: (local-projects-searcher system-name)
```

This function is added to ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*
to use the local project directory and cache to find systems.

### provided-systems

```lisp
Generic Function: (provided-systems object)
```

Return a list of systems provided by `object`.

### qmerge

```lisp
Function: (qmerge pathname)
```

Return `pathname` merged with the base Quicklisp directory.

### quickload

```lisp
Generic Function: (quickload systems &key verbose silent prompt explain
                   &allow-other-keys)
```

Load `systems` the quicklisp way. `systems` is a designator for a list
   of things to be loaded.

### register-local-projects

```lisp
Function: (register-local-projects)
```

Force a scan of the local projects directory to create the system
file index.

### setup

```lisp
Function: (setup)
```



### system-apropos

```lisp
Generic Function: (system-apropos term)
```



### system-apropos-list

```lisp
Generic Function: (system-apropos-list term)
```



### system-list

```lisp
Function: (system-list)
```



### system-not-found

```lisp
Condition
```

This condition is signaled by QUICKLOAD when a
  system given to load is not available via ASDF or a Quicklisp
  dist.

<u>**Direct Slots**</u>

**ql-dist:name**
```lisp
Initargs: :NAME
Readers: SYSTEM-NOT-FOUND-NAME
```

### system-not-found-name

```lisp
Generic Function: (system-not-found-name condition)
```



### uninstall

```lisp
Function: (uninstall system-name)
```



### uninstall-dist

```lisp
Function: (uninstall-dist name)
```



### update-all-dists

```lisp
Function: (update-all-dists &key (prompt t))
```



### update-client

```lisp
Function: (update-client &key (prompt t))
```



### update-dist

```lisp
Function: (update-dist dist &key (prompt t))
```



### use-only-quicklisp-systems

```lisp
Function: (use-only-quicklisp-systems)
```



### where-is-system

```lisp
Function: (where-is-system name)
```

Return the pathname to the source directory of ASDF system with the
given `name`, or NIL if no system by that name can be found known.

### who-depends-on

```lisp
Function: (who-depends-on system-name)
```

Return a list of names of systems that depend on `system-name`.

### write-asdf-manifest-file

```lisp
Function: (write-asdf-manifest-file output-file &key
           (if-exists :rename-and-delete) exclude-local-projects)
```

Write a list of system file pathnames to `output-file`, one per line,
in order of descending QL-DIST:PREFERENCE.


## ql-dist: API Reference

Generic functions, variables, and classes for interacting with the
   dist system. Documented, exported symbols are intended for public
   use.


### \*dist-enumeration-functions\*

```lisp
Variable
```

[all-dists](#all-dists) calls each function in this list with no arguments, and
  appends the results into a list of dist objects, removing
  duplicates. Functions might be called just once for a batch of
  related operations; see [with-consistent-dists](#with-consistent-dists).


### all-dists

```lisp
Function: (all-dists)
```

Return a list of all known dists.

### archive-content-sha1

```lisp
Generic Function: (archive-content-sha1 object)
```



### archive-md5

```lisp
Generic Function: (archive-md5 object)
```



### archive-size

```lisp
Generic Function: (archive-size object)
```



### archive-url

```lisp
Generic Function: (archive-url release)
```

Return the full URL for fetching the archive file of `release`.

### available-update

```lisp
Generic Function: (available-update dist)
```

If an update is available for `dist`, return the
  update as an uninstalled dist object. Otherwise, return NIL.

### available-versions

```lisp
Generic Function: (available-versions object)
```

Return a list of version information for `object`.

### available-versions-url

```lisp
Generic Function: (available-versions-url object)
```

Return the URL for the available versions data file of `object`.

### badly-sized-local-archive

```lisp
Condition
```


### base-directory

```lisp
Generic Function: (base-directory object)
```

Return the base directory pathname of `object`.

### canonical-distinfo-url

```lisp
Generic Function: (canonical-distinfo-url object)
```



### check-local-archive-file

```lisp
Generic Function: (check-local-archive-file release)
```

Check the local archive file of `release` for validity, including
   size and signature checks. Signals errors in the case of invalid files.

### clean

```lisp
Generic Function: (clean object)
```

Remove any unneeded files or directories related to
  `object`.

### dependency-tree

```lisp
Generic Function: (dependency-tree system)
```



### disable

```lisp
Generic Function: (disable object)
```

Disable `object`.

### dist

```lisp
Generic Function: (dist object)
```

Return the dist of `object`.

```lisp
Class
```

<u>**Direct Slots**</u>

**base-directory**
```lisp
Initargs: :BASE-DIRECTORY
Readers: BASE-DIRECTORY
Writers: (SETF BASE-DIRECTORY)
```
**name**
```lisp
Initargs: :NAME
Readers: NAME
Writers: (SETF NAME)
```
**version**
```lisp
Initargs: :VERSION
Readers: VERSION
Writers: (SETF VERSION)
```
**system-index-url**
```lisp
Initargs: :SYSTEM-INDEX-URL
Readers: SYSTEM-INDEX-URL
Writers: (SETF SYSTEM-INDEX-URL)
```
**release-index-url**
```lisp
Initargs: :RELEASE-INDEX-URL
Readers: RELEASE-INDEX-URL
Writers: (SETF RELEASE-INDEX-URL)
```
**available-versions-url**
```lisp
Initargs: :AVAILABLE-VERSIONS-URL
Readers: AVAILABLE-VERSIONS-URL
Writers: (SETF AVAILABLE-VERSIONS-URL)
```
**canonical-distinfo-url**
```lisp
Initargs: :CANONICAL-DISTINFO-URL
Readers: CANONICAL-DISTINFO-URL
Writers: (SETF CANONICAL-DISTINFO-URL)
```
**provided-systems**
```lisp
Initargs: :PROVIDED-SYSTEMS
Readers: PROVIDED-SYSTEMS
Writers: (SETF PROVIDED-SYSTEMS)
```
**provided-releases**
```lisp
Initargs: :PROVIDED-RELEASES
Readers: PROVIDED-RELEASES
Writers: (SETF PROVIDED-RELEASES)
```

### enable

```lisp
Generic Function: (enable object)
```

Enable `object`.

### enabled-dists

```lisp
Function: (enabled-dists)
```

Return a list of all known dists for which [enabledp](#enabledp) returns true.

### enabledp

```lisp
Generic Function: (enabledp object)
```

Return true if `object` is enabled.

### ensure-installed

```lisp
Generic Function: (ensure-installed object)
```

Ensure that `object` is installed.

### ensure-local-archive-file

```lisp
Generic Function: (ensure-local-archive-file release)
```

If the archive file for `release` is not available locally, fetch it
   and return the pathname to it.

### find-asdf-system-file

```lisp
Function: (find-asdf-system-file name)
```

Return the ASDF system file in which the system named `name` is defined.

### find-dist

```lisp
Function: (find-dist name)
```



### find-dist-or-lose

```lisp
Function: (find-dist-or-lose name)
```



### find-release

```lisp
Generic Function: (find-release name)
```

Return a release with the given `name`, or NIL if no system is
   found. If multiple releases have the same name, the one with the
   highest preference is returned.

### find-release-in-dist

```lisp
Generic Function: (find-release-in-dist release-name dist)
```

Return a release with the given [name](#name) in `dist`, or NIL if no release
   is found.

### find-system

```lisp
Generic Function: (find-system name)
```

Return a system with the given `name`, or NIL if no system is
   found. If multiple systems have the same name, the one with the
   highest preference is returned.

### find-system-in-dist

```lisp
Generic Function: (find-system-in-dist system-name dist)
```

Return a system with the given [name](#name) in `dist`, or NIL if no system
   is found.

### forget-preference

```lisp
Generic Function: (forget-preference object)
```

Remove specific preference information for `object`.

### inhibit-subscription

```lisp
Generic Function: (inhibit-subscription object)
```

Inhibit subscription for `object`.

### initialize-release-index

```lisp
Generic Function: (initialize-release-index dist)
```

Initialize the release index of `dist`.

### initialize-system-index

```lisp
Generic Function: (initialize-system-index dist)
```

Initialize the system index of `dist`.

### install

```lisp
Generic Function: (install object)
```

Install `object`.

### install-dist

```lisp
Function: (install-dist url &key (prompt t) replace)
```



### install-metadata-file

```lisp
Generic Function: (install-metadata-file object)
```

The pathname to a file describing the installation status of
   `object`.

### installed-releases

```lisp
Generic Function: (installed-releases dist)
```

Return a list of all releases installed for `dist`.

### installed-systems

```lisp
Generic Function: (installed-systems dist)
```

Return a list of all systems installed for `dist`.

### installedp

```lisp
Generic Function: (installedp object)
```

Return true if `object` is installed.

### invalid-local-archive

```lisp
Condition
```

<u>**Direct Slots**</u>

**release**
```lisp
Initargs: :RELEASE
Readers: INVALID-LOCAL-ARCHIVE-RELEASE
```

### invalid-local-archive-file

```lisp
Generic Function: (invalid-local-archive-file condition)
```



### invalid-local-archive-release

```lisp
Generic Function: (invalid-local-archive-release condition)
```



### local-archive-file

```lisp
Generic Function: (local-archive-file release)
```

Return the pathname to where the archive file of `release` should be
   stored.

### metadata-name

```lisp
Generic Function: (metadata-name object)
```

The metadata-name of an object is used to form the pathname for a
   few different object metadata files.

### missing-local-archive

```lisp
Condition
```


### name

```lisp
Generic Function: (name object)
```

Return the name of `object`.

### new-version-available-p

```lisp
Generic Function: (new-version-available-p dist)
```

Return true if a new version of `dist` is available.

### preference

```lisp
Generic Function: (preference object)
```

Returns a value used when comparing multiple systems or releases
   with the same name. Objects with higher preference are returned by
   [find-system](#find-system) and [find-release](#find-release).

### preference-file

```lisp
Generic Function: (preference-file object)
```

Return the file from which preference information is loaded for
   `object`.

### preference-parent

```lisp
Generic Function: (preference-parent object)
```

Return a value suitable for checking if `object` has no specific
   preference set.

### prefix

```lisp
Generic Function: (prefix object)
```



### project-name

```lisp
Generic Function: (project-name object)
```



### provided-releases

```lisp
Generic Function: (provided-releases object)
```

Return a list of releases provided by `object`.

### provided-systems

```lisp
Generic Function: (provided-systems object)
```

Return a list of systems provided by `object`.

### relative-to

```lisp
Generic Function: (relative-to object pathname)
```

Merge `pathname` with the base-directory of `object`.

### release

```lisp
Generic Function: (release object)
```

Return the release of `object`.

```lisp
Class
```

Instances of this class represent a snapshot of a project at some
   point in time, which might be from version control, or from an
   official release, or from some other source.

<u>**Direct Slots**</u>

**project-name**
```lisp
Initargs: :PROJECT-NAME
```
**dist**
```lisp
Initargs: :DIST
```
**provided-systems**
```lisp
Initargs: :PROVIDED-SYSTEMS
Readers: PROVIDED-SYSTEMS
Writers: (SETF PROVIDED-SYSTEMS)
```
**archive-url**
```lisp
Initargs: :ARCHIVE-URL
Readers: ARCHIVE-URL
Writers: (SETF ARCHIVE-URL)
```
**archive-size**
```lisp
Initargs: :ARCHIVE-SIZE
Readers: ARCHIVE-SIZE
Writers: (SETF ARCHIVE-SIZE)
```
**archive-md5**
```lisp
Initargs: :ARCHIVE-MD5
Readers: ARCHIVE-MD5
Writers: (SETF ARCHIVE-MD5)
```
**archive-content-sha1**
```lisp
Initargs: :ARCHIVE-CONTENT-SHA1
Readers: ARCHIVE-CONTENT-SHA1
Writers: (SETF ARCHIVE-CONTENT-SHA1)
```
**prefix**
```lisp
Initargs: :PREFIX
```
**system-files**
```lisp
Initargs: :SYSTEM-FILES
Readers: SYSTEM-FILES
Writers: (SETF SYSTEM-FILES)
```
**metadata-name**
```lisp
Initargs: :METADATA-NAME
Readers: METADATA-NAME
Writers: (SETF METADATA-NAME)
```

### release-index-url

```lisp
Generic Function: (release-index-url object)
```

Return the URL for the release index of `object`.

### required-systems

```lisp
Generic Function: (required-systems object)
```



### short-description

```lisp
Generic Function: (short-description object)
```

Return a short string describing `object`.

### show-update-report

```lisp
Generic Function: (show-update-report old-dist new-dist)
```

Display a description of the update from `old-dist`
  to `new-dist`.

### standard-dist-enumeration-function

```lisp
Function: (standard-dist-enumeration-function)
```

The default function used for producing a list of dist objects.

### subscribe

```lisp
Generic Function: (subscribe object)
```

Subscribe to updates of `object`, if possible. If no
  updates are available, a condition of type [subscription-unavailable](#subscription-unavailable)
  is raised.

### subscribedp

```lisp
Generic Function: (subscribedp object)
```

Return true if `object` is subscribed to updates.

### subscription-inhibited-p

```lisp
Generic Function: (subscription-inhibited-p object)
```

Return T if subscription to `object` is inhibited.

### subscription-inhibition-file

```lisp
Generic Function: (subscription-inhibition-file object)
```

The file whose presence indicates the inhibited
  subscription status of `object`.

### subscription-unavailable

```lisp
Condition
```


### subscription-url

```lisp
Generic Function: (subscription-url object)
```



### system

```lisp
Generic Function: (system object)
```

Return the system of `object`.

```lisp
Class
```

<u>**Direct Slots**</u>

**name**
```lisp
Initargs: :NAME
```
**system-file-name**
```lisp
Initargs: :SYSTEM-FILE-NAME
Readers: SYSTEM-FILE-NAME
Writers: (SETF SYSTEM-FILE-NAME)
```
**release**
```lisp
Initargs: :RELEASE
```
**dist**
```lisp
Initargs: :DIST
Readers: DIST
Writers: (SETF DIST)
```
**required-systems**
```lisp
Initargs: :REQUIRED-SYSTEMS
Readers: REQUIRED-SYSTEMS
Writers: (SETF REQUIRED-SYSTEMS)
```
**metadata-name**
```lisp
Initargs: :METADATA-NAME
Readers: METADATA-NAME
Writers: (SETF METADATA-NAME)
```

### system-apropos

```lisp
Generic Function: (system-apropos term)
```



### system-apropos-list

```lisp
Generic Function: (system-apropos-list term)
```



### system-definition-searcher

```lisp
Function: (system-definition-searcher name)
```

Like [find-asdf-system-file](#find-asdf-system-file), but this function can be used in
ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*; it will only return system
file names if they match `name`.

### system-file-name

```lisp
Generic Function: (system-file-name object)
```



### system-files

```lisp
Generic Function: (system-files object)
```



### system-index-url

```lisp
Generic Function: (system-index-url object)
```

Return the URL for the system index of `object`.

### uninhibit-subscription

```lisp
Generic Function: (uninhibit-subscription object)
```

Remove inhibition of subscription for `object`.

### uninstall

```lisp
Generic Function: (uninstall object)
```

Uninstall `object`.

### unknown-dist

```lisp
Condition
```

<u>**Direct Slots**</u>

**name**
```lisp
Initargs: :NAME
Readers: :UNKNOWN-DIST-NAME
```

### unsubscribe

```lisp
Generic Function: (unsubscribe object)
```

Unsubscribe from updates to `object`.

### update-in-place

```lisp
Generic Function: (update-in-place old-dist new-dist)
```

Update `old-dist` to `new-dist` in place.

### update-release-differences

```lisp
Generic Function: (update-release-differences old-dist new-dist)
```

Compare `old-dist` to `new-dist` and return three lists
  as multiple values: new releases (present in `new-dist` but not
  `old-dist`), changed releases (present in both dists but different in
  some way), and removed releases (present in `old-dist` but not
  `new-dist`). The list of changed releases is a list of two-element
  lists, with each two-element list having first the old release
  object and then the new release object.

### version

```lisp
Generic Function: (version object)
```



### with-consistent-dists

```lisp
Macro: (with-consistent-dists &body body)
```

See CALL-WITH-CONSISTENT-DISTS.
