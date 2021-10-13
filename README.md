# cabalgc: Selectively remove library packages in cabal store.

This program can list library package IDs in the cabal store and remove some of them.

The current version makes a Linux assumption: the cabal store is at
$HOME/.cabal/store.

Here is the --help message:

```
Usage: cabalgc [OPTION...] PKGID...
Remove library packages except those you specify and dependencies.
BUT: Dry-run unless you say -y or --yes .
  -h          --help         this help message
  -g VERSION  --ghc=VERSION  GHC version, e.g., 8.10 or 8.10.7
  -l          --list         just list packages, remove nothing
  -y          --yes          perform the removals (default is dry-run)
```

## Listing

`cabalgc --list` lists the library package IDs. Example:

```
$ cabalgc --list
attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18
attoparsec-0.14.1-8b967698b8ca9b4cfe0876733a65a24bb7481895dfa83030374e8eda687c259d
cassava-0.5.2.0-7348abe88993efbfaa6c704ca0236811f232f3553e644a9baa5d0ab588bc6f60
cassava-0.5.2.0-7549efff02995d832ef345c7eb7e75b62d4848492b85ae6a99c8bebb88471908
hashable-1.3.3.0-06faacb7b1e623eab0175fd0a560256155d4f1277f2fab474a5544596f28370a
hashable-1.3.4.1-87220b7404d7a82793f40abb4dd7d8ff0d0def24c7c96ff17fa4284821292074
integer-logarithms-1.0.3.1-929e94691dec6feec9fc7092bda83c34e1302e44dccd015b0e9dfc10770dbd0c
Only-0.1-801d0721d029d3024c3f3de24b698312df2655aa6930d2154f75af1df47724c9
primitive-0.7.2.0-88110134b23652b88f09a13c0f01344f82c38fa0b35c11be01aa12eb96139632
random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864
scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
scientific-0.3.7.0-71a81872275dd1245b55f1720c97b61dbd8313241c663a9c37295e78ba23e760
splitmix-0.1.0.3-e01ea87704740bf1a26e8c25b66b8cd9e76e225b6d6dc0adc01c92cc9b8224bd
text-short-0.1.3-b265b6e2a45a9a0f502267dd103f1b4cb15111abf54b20eb8d61345a6161dc08
text-short-0.1.3-c30b0ea2fb0523076a7d3683b0565e38b6a58d9a0498fd127dcd7ea25ae5e389
unordered-containers-0.2.14.0-773d2842b03fea62339d4774b8c0314bc161b10a29ba28caae844c53b4111c93
unordered-containers-0.2.14.0-9a72085d448f193007bf54371022077fecd5e44f98558da2c5d4d8aadbf53ab5
vector-0.12.3.1-997d26736edf6bf9c1a52f95dff2705f3e5975aab5d28a6a564d6a797298a316
```

## Removing

Removing works like garbage collection: You say what you want to keep, then this program
also figures out transitive dependencies to keep, and removes the rest.

This program dry-runs by default, i.e., just reports what would be removed, without actual
removal. For actual removal, add the -y or --yes option.

Example: I have two builds of cassava-0.5.2.0 (and basically all of its transitive dependencies)
because one was upon hashable-1.3.3.0 and later the other was upon hashable-1.3.4.1.

Naturally I want to keep the newer cassava build; I also want to keep random
(transitive dependencies: splitmix).  Here is a dry-run:

```
$ cabalgc \
>   cassava-0.5.2.0-7348abe88993efbfaa6c704ca0236811f232f3553e644a9baa5d0ab588bc6f60 \
>   random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864
Would remove cassava-0.5.2.0-7549efff02995d832ef345c7eb7e75b62d4848492b85ae6a99c8bebb88471908
Would remove unordered-containers-0.2.14.0-9a72085d448f193007bf54371022077fecd5e44f98558da2c5d4d8aadbf53ab5
Would remove text-short-0.1.3-c30b0ea2fb0523076a7d3683b0565e38b6a58d9a0498fd127dcd7ea25ae5e389
Would remove attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18
Would remove scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
Would remove hashable-1.3.3.0-06faacb7b1e623eab0175fd0a560256155d4f1277f2fab474a5544596f28370a
```

To actually remove, add -y or --yes:

```
$ cabalgc --yes \
>   cassava-0.5.2.0-7348abe88993efbfaa6c704ca0236811f232f3553e644a9baa5d0ab588bc6f60 \
>   random-1.2.1-e2a470bddae1da56736c04a4f90f71d380a26095b22e0ef3e73f4aa440aab864
Removing cassava-0.5.2.0-7549efff02995d832ef345c7eb7e75b62d4848492b85ae6a99c8bebb88471908
Removing unordered-containers-0.2.14.0-9a72085d448f193007bf54371022077fecd5e44f98558da2c5d4d8aadbf53ab5
Removing text-short-0.1.3-c30b0ea2fb0523076a7d3683b0565e38b6a58d9a0498fd127dcd7ea25ae5e389
Removing attoparsec-0.14.1-096bc01737db3644622b4f9b5113275dd490141a347b670fda6576d3b31fad18
Removing scientific-0.3.7.0-673ffad7b24354b484b4c1b2d89fa439e2b801d1766a523f1cd214e672e234f9
Removing hashable-1.3.3.0-06faacb7b1e623eab0175fd0a560256155d4f1277f2fab474a5544596f28370a
```

The removal order fulfills the safety constraint of `ghc-pkg unregister` (and common sense)
that if x depends on y, then x is removed before y.
