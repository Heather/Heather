VCS Synchronization Utility / Automatic rebase / Build
=========

[![Build Status](https://travis-ci.org/Heather/Sharingan.png?branch=master)](https://travis-ci.org/Heather/Sharingan)
[![Build status](https://ci.appveyor.com/api/projects/status/2b84cqnvh46xxpnv?svg=true)](https://ci.appveyor.com/project/Heather/sharingan)
![Build Status](https://codeship.com/projects/6b402750-06c3-0133-231f-2aa9a23a545f/status?branch=master)
 [![Twitter][]](http://www.twitter.com/Cynede)

```
Uchiha Dojutsu Kekkei Genkai [Mirror Wheel Eye]

Usage: sharingan [-v|--verbose] [-j|--jobs JOBS] ([COMMAND] | [--full]
                 [-f|--force] [-u|--unsafe] [-q|--quick] [-i|--interactive]
                 [--no-push] [FILTER] [-g|--group GROUPS]) [--version]

Available options:
  -v,--verbose             Set verbosity to LEVEL
  -j,--jobs JOBS           Maximum parallel jobs
  --version                Print version information
  -h,--help                Show this help text

Available commands:
  sync                     Process synchronization
  make                     Create .sharingan.yml template
  config                   Edit Sharingan Network config
  defaults                 Edit Sharingan Defaults config
  list                     List Sharingan Network
  status                   Display Sharingan build status for Network
  add                      Add repository to Sharingan Network (current path w/o
                           args)
  delete                   Delete repository from Sharingan Network (current
                           path w/o args)
  enable                   Enable repository / repositories
  disable                  Disable repository / repositories

```

a bit outdated per-repository config example (`.sharingan.yml`):
```yaml
script:
    - bash configure --disable-debug
    - make
```

a bit outdated config example (`sharingan.yml`):

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

[Twitter]: http://mxtoolbox.com/Public/images/twitter-icon.png
