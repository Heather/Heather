Sharingan
=========

[![Build Status](https://travis-ci.org/Heather/Sharingan.png?branch=master)](https://travis-ci.org/Heather/Sharingan)
[![Build status](https://ci.appveyor.com/api/projects/status/2b84cqnvh46xxpnv?svg=true)](https://ci.appveyor.com/project/Heather/sharingan)
![Build Status](https://codeship.com/projects/6b402750-06c3-0133-231f-2aa9a23a545f/status?branch=master)
<br/>
[![Twitter][]](http://www.twitter.com/Cynede)
![](http://fc01.deviantart.net/fs70/f/2011/188/d/2/ember_mangekyou_sharingan_by_jinseiasakura-d3lcdmk.png)

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
