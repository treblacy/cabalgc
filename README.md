# cabalgc: Selectively remove library packages in cabal store.

This program can list library package IDs in the cabal store and remove some of them.

Here is the help message:

```
Usage: cabalgc [OPTION...] PKGID...
Remove library packages except those you specify and transitive dependencies.
BUT: Dry-run unless you say -y or --yes .
  -h          --help            this help message
  -V          --version         print version number
  -g VERSION  --ghc=VERSION     GHC version, e.g., 8.10 or 8.10.7
  -l          --list            just list packages, remove nothing
  -d          --deps            just list dependencies, remove nothing
  -r          --rdeps           just list reverse dependencies, remove nothing
  -t          --tops            just list packages not depended on, remove nothing
              --fullhash[=y|n]  full hashes when printing package IDs
  -x          --remove          remove only specified packages (dry-run default applies)
  -y          --yes             perform the removals (default is dry-run)
```

## Listing packages, dependencies, etc.

`cabalgc --list` lists the library package IDs. Example:

```
$ cabalgc --list
base-orphans-0.9.0-97381d0
data-array-byte-0.1.0.1-faf1e8a
hmatrix-0.20.2-56cbf16
network-3.1.4.0-896c3fb
primitive-0.8.0.0-0bc4833
random-1.2.1.1-18ed557
semigroups-0.20-74fef4d
split-0.2.3.5-402d901
splitmix-0.1.0.4-64be883
storable-complex-0.2.3.0-0b89ddc
vector-0.13.0.0-16ca557
vector-stream-0.1.0.0-2fa3fc9
```

By default, only 7 digits of the hashes are shown.  If you want to see full
hashes, add the `--fullhash` option.  Note that you will not need the full hash
for the other functions of this program—it performs completion for prefixes of
package IDs you give.

`cabalgc --tops` lists packages at “the top of the food chain”, i.e., the rest
of the cabal store doesn't depend on them:

```
$ cabalgc --tops
hmatrix-0.20.2-56cbf16
network-3.1.4.0-896c3fb
```

`cabalgc --deps PKGID...` lists the dependency forest that starts from the
specified packages (but confined to the cabal store, e.g., base is not listed as
a dependency).  If no package is specified, the whole store is listed.

```
$ cabalgc --deps vector-0 random
data-array-byte-0.1.0.1-faf1e8a ->
    ;
primitive-0.8.0.0-0bc4833 ->
    data-array-byte-0.1.0.1-faf1e8a
    ;
random-1.2.1.1-18ed557 ->
    splitmix-0.1.0.4-64be883
    ;
splitmix-0.1.0.4-64be883 ->
    ;
vector-0.13.0.0-16ca557 ->
    primitive-0.8.0.0-0bc4833
    vector-stream-0.1.0.0-2fa3fc9
    ;
vector-stream-0.1.0.0-2fa3fc9 ->
    ;
```

The output format tries to be nice to both humans and machines. For humans,
there are indentations and almost-blank lines.  For machines, there are “`->`”
and “`;`” separators.

`cabalgc --rdeps PKGID...` lists the reverse dependency forest that starts from
the specified packages (but again confined to the cabal store).  If no package
is specified, the whole store is listed.

The output format is similar to `--deps` but with “`<-`” instead.

```
$ cabalgc --rdeps split-0 splitmix
hmatrix-0.20.2-56cbf16 <-
    ;
random-1.2.1.1-18ed557 <-
    hmatrix-0.20.2-56cbf16
    ;
split-0.2.3.5-402d901 <-
    hmatrix-0.20.2-56cbf16
    ;
splitmix-0.1.0.4-64be883 <-
    random-1.2.1.1-18ed557
    ;
```


## Removing

### Removing named packages only

The `--remove` option is for removing only the packages you name.  This program
will compute a safe removal order.  There is a safety check: A named package is
kept if it is needed by an unremoved package.  (And of course a package ID that
doesn't exist cannot be removed either.)  A dry-run is done, i.e., just reports
what would be removed; for actual removal, add `-y` or `--yes`.

For kept packages, warnings are given to list the root causes: all top packages
(only—for brevity) that transitively depend on the kept packages.  (To see all
intermediate packages, you can use `cabalgc --rdeps PKGID...`.)

Dry-run example:

```
$ cabalgc --remove network splitmix foobar
```

Output on stdout: What would be removed:

```
Would remove network-3.1.4.0-896c3fb
```

Warnings on stderr: The Unremoved and the non-existent:

```
Warning: splitmix-0.1.0.4-64be883 not removed: needed by: hmatrix-0.20.2-56cbf16
Warning: foobar not removed: not in the cabal store.
```

Removal example:

```
$ cabalgc --remove --yes network splitmix foobar
```

Output on stdout:

```
Removing network-3.1.4.0-896c3fb
```

Warnings on stderr:

```
Warning: splitmix-0.1.0.4-64be883 not removed: needed by: hmatrix-0.20.2-56cbf16
Warning: foobar not removed: not in the cabal store.
```


### Removing unamed packages (garbage collection)

The default mode works like garbage collection: You say what you want to keep.
(Hopefully, the various listing options above help you explore and decide.)
Then this program also figures out transitive dependencies to keep, and removes
the rest.

This program dry-runs by default, i.e., just reports what would be removed; for
actual removal, add `-y` or `--yes`.

Example: I want to keep random and vector.  Here is a dry-run:

```
$ cabalgc random vector-0
Would remove network-3.1.4.0-896c3fb
Would remove hmatrix-0.20.2-56cbf16
Would remove storable-complex-0.2.3.0-0b89ddc
Would remove split-0.2.3.5-402d901
Would remove semigroups-0.20-74fef4d
Would remove base-orphans-0.9.0-97381d0
```

To actually remove, add -y or --yes:

```
$ cabalgc --yes random vector-0
Removing network-3.1.4.0-896c3fb
Removing hmatrix-0.20.2-56cbf16
Removing storable-complex-0.2.3.0-0b89ddc
Removing split-0.2.3.5-402d901
Removing semigroups-0.20-74fef4d
Removing base-orphans-0.9.0-97381d0
```

The removal order fulfills the safety constraint of `ghc-pkg unregister` (and common sense)
that if x depends on y, then x is removed before y.
