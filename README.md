# cabalgc: Selectively remove library packages in cabal store.

This program can list library package IDs in the cabal store and remove some of them.

The current version makes a Linux assumption: the cabal store is at
$HOME/.cabal/store.

Here is the --help message:

```
Usage: cabalgc [OPTION...] PKGID...
Remove library packages except those you specify and transitive dependencies.
BUT: Dry-run unless you say -y or --yes .
  -h          --help         this help message
  -g VERSION  --ghc=VERSION  GHC version, e.g., 8.10 or 8.10.7
  -l          --list         just list packages, remove nothing
  -d          --deps         just list dependencies, remove nothing
  -r          --rdeps        just list reverse dependencies, remove nothing
  -t          --tops         just list packages not depended on, remove nothing
  -y          --yes          perform the removals (default is dry-run)
```

## Listing packages, dependencies, etc.

`cabalgc --list` lists the library package IDs. Example:

```
$ cabalgc --list
attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18
attoparsec-0.14.1-8b967698b8ca9b4cfe0876733a65a24bb7481895dfa83030374e8eda687c259d
hashable-1.3.3.0-06faacb7b1e623eab0175fd0a560256155d4f1277f2fab474a5544596f28370a
hashable-1.3.4.1-87220b7404d7a82793f40abb4dd7d8ff0d0def24c7c96ff17fa4284821292074
integer-logarithms-1.0.3.1-929e94691dec6feec9fc7092bda83c34e1302e44dccd015b0e9dfc10770dbd0c
primitive-0.7.2.0-88110134b23652b88f09a13c0f01344f82c38fa0b35c11be01aa12eb96139632
random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864
scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
scientific-0.3.7.0-71a81872275dd1245b55f1720c97b61dbd8313241c663a9c37295e78ba23e760
splitmix-0.1.0.3-e01ea87704740bf1a26e8c25b66b8cd9e76e225b6d6dc0adc01c92cc9b8224bd
```

`cabalgc --tops` lists packages at “the top of the food chain”, i.e., the rest
of the cabal store doesn't depend on them:

```
$ cabalgc --tops
attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18
attoparsec-0.14.1-8b967698b8ca9b4cfe0876733a65a24bb7481895dfa83030374e8eda687c259d
random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864
```

`cabalgc --deps` lists [direct] dependencies (but confined to what's in the
cabal store, e.g., base is not listed as a dependency):

```
$ cabalgc --deps
attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18 ->
    scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
    ;
attoparsec-0.14.1-8b967698b8ca9b4cfe0876733a65a24bb7481895dfa83030374e8eda687c259d ->
    scientific-0.3.7.0-71a81872275dd1245b55f1720c97b61dbd8313241c663a9c37295e78ba23e760
    ;
hashable-1.3.3.0-06faacb7b1e623eab0175fd0a560256155d4f1277f2fab474a5544596f28370a ->
    ;
hashable-1.3.4.1-87220b7404d7a82793f40abb4dd7d8ff0d0def24c7c96ff17fa4284821292074 ->
    ;
integer-logarithms-1.0.3.1-929e94691dec6feec9fc7092bda83c34e1302e44dccd015b0e9dfc10770dbd0c ->
    ;
primitive-0.7.2.0-88110134b23652b88f09a13c0f01344f82c38fa0b35c11be01aa12eb96139632 ->
    ;
random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864 ->
    splitmix-0.1.0.3-e01ea87704740bf1a26e8c25b66b8cd9e76e225b6d6dc0adc01c92cc9b8224bd
    ;
scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9 ->
    hashable-1.3.3.0-06faacb7b1e623eab0175fd0a560256155d4f1277f2fab474a5544596f28370a
    integer-logarithms-1.0.3.1-929e94691dec6feec9fc7092bda83c34e1302e44dccd015b0e9dfc10770dbd0c
    primitive-0.7.2.0-88110134b23652b88f09a13c0f01344f82c38fa0b35c11be01aa12eb96139632
    ;
scientific-0.3.7.0-71a81872275dd1245b55f1720c97b61dbd8313241c663a9c37295e78ba23e760 ->
    hashable-1.3.4.1-87220b7404d7a82793f40abb4dd7d8ff0d0def24c7c96ff17fa4284821292074
    integer-logarithms-1.0.3.1-929e94691dec6feec9fc7092bda83c34e1302e44dccd015b0e9dfc10770dbd0c
    primitive-0.7.2.0-88110134b23652b88f09a13c0f01344f82c38fa0b35c11be01aa12eb96139632
    ;
splitmix-0.1.0.3-e01ea87704740bf1a26e8c25b66b8cd9e76e225b6d6dc0adc01c92cc9b8224bd ->
    ;
```

The output format tries to be nice to both humans and machines. For humans,
there are indentations and almost-blank lines.  For machines, there are “`->`”
and “`;`” separators.

`cabalgc --rdeps` lists [direct] reverse dependencies (but again confined to the
cabal store):

```
$ cabalgc --rdeps
attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18 <-
    ;
attoparsec-0.14.1-8b967698b8ca9b4cfe0876733a65a24bb7481895dfa83030374e8eda687c259d <-
    ;
hashable-1.3.3.0-06faacb7b1e623eab0175fd0a560256155d4f1277f2fab474a5544596f28370a <-
    scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
    ;
hashable-1.3.4.1-87220b7404d7a82793f40abb4dd7d8ff0d0def24c7c96ff17fa4284821292074 <-
    scientific-0.3.7.0-71a81872275dd1245b55f1720c97b61dbd8313241c663a9c37295e78ba23e760
    ;
integer-logarithms-1.0.3.1-929e94691dec6feec9fc7092bda83c34e1302e44dccd015b0e9dfc10770dbd0c <-
    scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
    scientific-0.3.7.0-71a81872275dd1245b55f1720c97b61dbd8313241c663a9c37295e78ba23e760
    ;
primitive-0.7.2.0-88110134b23652b88f09a13c0f01344f82c38fa0b35c11be01aa12eb96139632 <-
    scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
    scientific-0.3.7.0-71a81872275dd1245b55f1720c97b61dbd8313241c663a9c37295e78ba23e760
    ;
random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864 <-
    ;
scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9 <-
    attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18
    ;
scientific-0.3.7.0-71a81872275dd1245b55f1720c97b61dbd8313241c663a9c37295e78ba23e760 <-
    attoparsec-0.14.1-8b967698b8ca9b4cfe0876733a65a24bb7481895dfa83030374e8eda687c259d
    ;
splitmix-0.1.0.3-e01ea87704740bf1a26e8c25b66b8cd9e76e225b6d6dc0adc01c92cc9b8224bd <-
    random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864
    ;
```

The output format is similar to `--deps` but with “`<-`” instead.


## Removing

Removing works like garbage collection: You say what you want to keep.
(Hopefully, the various listing options above help you explore and decide.)
Then this program also figures out transitive dependencies to keep, and removes
the rest.

This program dry-runs by default, i.e., just reports what would be removed, without actual
removal. For actual removal, add the -y or --yes option.

Example: I have two builds of attoparsec-0.14.1 (and most of its transitive
dependencies) because one was built upon hashable-1.3.3.0 and the other was
built later upon hashable-1.3.4.1.

I want to keep random and the newer attoparsec build.  Here is a dry-run:

```
$ cabalgc \
>   attoparsec-0.14.1-8b967698b8ca9b4cfe0876733a65a24bb7481895dfa83030374e8eda687c259d \
>   random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864
Would remove attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18
Would remove scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
Would remove hashable-1.3.3.0-06faacb7b1e623eab0175fd0a560256155d4f1277f2fab474a5544596f28370a
```

To actually remove, add -y or --yes:

```
$ cabalgc --yes \
>   attoparsec-0.14.1-8b967698b8ca9b4cfe0876733a65a24bb7481895dfa83030374e8eda687c259d \
>   random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864
Removing attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18
Removing scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
Removing hashable-1.3.3.0-06faacb7b1e623eab0175fd0a560256155d4f1277f2fab474a5544596f28370a
```

The removal order fulfills the safety constraint of `ghc-pkg unregister` (and common sense)
that if x depends on y, then x is removed before y.
