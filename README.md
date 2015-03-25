Sharingan
=========

[![Build Status](https://travis-ci.org/Heather/Sharingan.png?branch=master)](https://travis-ci.org/Heather/Sharingan)

 - Sharingan is not CI
 - Sharingan doesn't need white IP address to handle github or another hooks
 - Sharingan is not a polling service, you run it when you want to run it
 - Sharingan is not just build, you also process rebasing and pushing, etc...
 - Sharingan is command line tool with random features (I can't imagine if I can separate other stuff by plugins or something like that)

![](http://fc01.deviantart.net/fs70/f/2011/188/d/2/ember_mangekyou_sharingan_by_jinseiasakura-d3lcdmk.png)

per-repository config example (`.sharingan.yml`):
```yaml
script:
    - bash configure --disable-debug
    - make
```

config example (`sharingan.yml`):

```yaml
- task: rebase
  group: null
  branches:
  - master
  hash: null
  location: D:\Heather\Contrib\P\optparse-applicative
  enabled: null
  clean: null
  upstream: upstream master
  postRebuild: null
- task: rebase
  group: null
  branches:
  - master
  - heather
  hash: 996acbdcf923f98b5623a79464ec80c2491cca53
  location: D:\TFS\winforms-modernui
  enabled: null
  clean: null
  upstream: upstream master
  postRebuild: null
```

usage example :

``` shell
D:\Heather\Contrib\P\H>sharingan sync coreu
 __________________________________________________________________________________________
D:\Heather\Contrib\P\coreutils <> master
Already on 'master'
No rebase in progress?
From github.com:Heather/coreutils
 * branch            master     -> FETCH_HEAD
Already up-to-date.
From github.com:uutils/coreutils
 * branch            master     -> FETCH_HEAD
From github.com:uutils/coreutils
 * branch            master     -> FETCH_HEAD
Current branch master is up to date.
Everything up-to-date
mkdir build
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/basename basename/basename.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/cat cat/cat.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/dirname dirname/dirname.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/echo echo/echo.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/env env/env.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/false false/false.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/printenv printenv/printenv.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/pwd pwd/pwd.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/rm rm/rm.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/rmdir rmdir/rmdir.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/sleep sleep/sleep.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/true true/true.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/wc wc/wc.rs'
sh -c 'D:\\Heather\\Contrib\\P\\rust\\i686-pc-mingw32\\stage2\\bin\\rustc.exe --opt-level=3 -o build/yes yes/yes.rs'
 __________________________________________________________________________________________
```

```shell
D:\Heather\Contrib\P\H>sharingan --help
Uchiha Dojutsu Kekkei Genkai [Mirror Wheel Eye]

Usage: sharingan [-v|--verbose] [-j|--jobs JOBS] COMMAND [--version]

Available options:
  -v,--verbose             Set verbosity to LEVEL
  -j,--jobs JOBS           Maximum parallel jobs
  --version                Print version information
  -h,--help                Show this help text

Available commands:
  sync                     Process synchronization
  make                     Create .sharingan.yml template
  config                   Edit .sharingan.yml config file
  defaults                 Edit .sharinganDefaults.yml config file
  list                     List repositories
  add                      Add repository (current path w/o args)
  delete                   Delete repository (current path w/o args)
  enable                   Enable repository / repositories
  disable                  Disable repository / repositories
  depot                    Get / Update Google depot tools with git and python
  cabal                    Cabal upgrade
```
