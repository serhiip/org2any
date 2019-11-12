# org2any
CLI utility to seamlessly synchronise org file items to other storages. Currently macOS Reminders App is only supported storage. Also contains integration package for Emacs

# Usage
```
Reminders helper

Usage: org2any FILEPATH (--destination DESTINATION | --default-destination)
               [-w|--watch] ([-v] | [-q|--quiet])
  Sync org file with MacOS Reminders

Available options:
  --destination DESTINATION
                           Where to save reminders parsed in FILEPATH
  --default-destination    Use default destination to store todos
  -w,--watch               Watch file changes and execute a sync on each change
  -v                       Log additional information to stdout
  -q,--quiet               Dont log any information to stdout
  -h,--help                Show this help text
```

There is also Emacs package that uses this `org2any` to trigger synchronization at the right time (when org file is saved). `org2any` binary should be placed on PATH and `org2any.el` loaded into Emacs.

# Installation
## From sources
Clone a repo and use one of the tools below

### `nix`
Use `nix-build release.nix` command. The `org2any` binary will be placed undor `./result/bin` folder

### `stack install`
The `org2any` binary will be copied to stack installation folder. That foldor should be on your PATH

# Architecture
An interpreter using [Free Monads](http://hackage.haskell.org/package/free) to abstract away access to particular reminders storage
```
                       ------------------
                  /-->( Mac OS Reminders )
              /---     ------------------
+-----+   /---         -----------------
| Org |<------------->( Mac OS Calendar )
+-----+  \----         -----------------
           \- \---     -----------------
             \-   \-->( Google Calendar )
               \-      -----------------
                 \-    -----------------
                   \->(       Other     )
                       -----------------
```
