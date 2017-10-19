Sharingan
=========

[![Build Status](https://travis-ci.org/Heather/Sharingan.png?branch=master)](https://travis-ci.org/Heather/Sharingan)
[![Build status](https://ci.appveyor.com/api/projects/status/2b84cqnvh46xxpnv?svg=true)](https://ci.appveyor.com/project/Heather/sharingan)
![Build Status](https://codeship.com/projects/6b402750-06c3-0133-231f-2aa9a23a545f/status?branch=master)
<br/>
[![Twitter][]](http://www.twitter.com/Cynede)
[![Donate Bitcoin](https://img.shields.io/badge/donate-bitcoin-orange.svg)](http://heather.github.io/donate-bitcoin/)
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

usage example :

``` shell
C:\Program Files (x86)\Microsoft Visual Studio 12.0>sharingan sync
I can't shove the dark out of my way.
C:/Program Files (x86)/Git/cmd/git.exe : git version 1.9.5.msysgit.0
 - C:\H\P\cabal : master
HEAD is now at dca1c96 Comment how to usefully use bootstrapping.
No rebase in progress?
C:/Program Files (x86)/Git/cmd/git.exe : 407d4c8e267e4468603e551aa4250de0158342a3       refs/heads/master
Last Merge: dca1c96a5c7f7a8c1408e007c0b1514a976aac87
Remote: 407d4c8e267e4468603e551aa4250de0158342a3
C:/Program Files (x86)/Git/cmd/git.exe : 09a71929e433f36b27fd6a4938469d3bbbd5e191       refs/heads/master
Origin: 09a71929e433f36b27fd6a4938469d3bbbd5e191
Local: dca1c96a5c7f7a8c1408e007c0b1514a976aac87
remote: Counting objects: 1192, done.
remote: Compressing objects: 100% (5/5), done.
Receiving objects:  99% (1181/1192), 316.00 KiB | 526.00 KiB/s   d 667R
Receiving objects: 100% (1192/1192), 565.71 KiB | 526.00 KiB/s, done.
Resolving deltas: 100% (822/822), completed with 186 local objects.
From github.com:Heather/cabal
 * branch            master     -> FETCH_HEAD
   dca1c96..09a7192  master     -> origin/master
Updating dca1c96..09a7192
Fast-forward
 .gitignore                                         |   3 +-
 .travis.yml                                        |   7 +-
 Cabal/Cabal.cabal                                  |  30 +-
 Cabal/Distribution/Compat/CopyFile.hs              |   6 +-
 Cabal/Distribution/Compat/Internal/TempFile.hs     |   2 +-
 Cabal/Distribution/Compiler.hs                     |   6 +-
 Cabal/Distribution/InstalledPackageInfo.hs         | 106 +--
 Cabal/Distribution/License.hs                      |   8 +-
 Cabal/Distribution/Make.hs                         |  11 +-
 Cabal/Distribution/ModuleName.hs                   |   5 +-
 Cabal/Distribution/Package.hs                      |  60 +-
 Cabal/Distribution/PackageDescription.hs           |  30 +-
 Cabal/Distribution/PackageDescription/Check.hs     |  91 +-
 .../PackageDescription/Configuration.hs            | 187 ++--
 Cabal/Distribution/PackageDescription/Parse.hs     |  41 +-
 .../Distribution/PackageDescription/PrettyPrint.hs |  23 +-
 Cabal/Distribution/ParseUtils.hs                   |  14 +-
 Cabal/Distribution/Simple.hs                       |  47 +-
 Cabal/Distribution/Simple/Bench.hs                 |  16 +-
 Cabal/Distribution/Simple/Build.hs                 |  72 +-
 Cabal/Distribution/Simple/Build/Macros.hs          |  18 +-
 Cabal/Distribution/Simple/Build/PathsModule.hs     |   8 -
 Cabal/Distribution/Simple/BuildPaths.hs            |  23 +-
 Cabal/Distribution/Simple/BuildTarget.hs           |  33 +-
 Cabal/Distribution/Simple/Command.hs               |  12 +-
 Cabal/Distribution/Simple/Compiler.hs              |  15 +-
 Cabal/Distribution/Simple/Configure.hs             | 390 ++++-----
 Cabal/Distribution/Simple/GHC.hs                   |  85 +-
 Cabal/Distribution/Simple/GHC/IPI641.hs            | 107 ---
 Cabal/Distribution/Simple/GHC/IPI642.hs            |  53 +-
 Cabal/Distribution/Simple/GHC/IPIConvert.hs        |  50 ++
 Cabal/Distribution/Simple/GHC/ImplInfo.hs          |   4 +-
 Cabal/Distribution/Simple/GHC/Internal.hs          |  38 +-
 Cabal/Distribution/Simple/GHCJS.hs                 |  51 +-
 Cabal/Distribution/Simple/Haddock.hs               |  77 +-
 Cabal/Distribution/Simple/HaskellSuite.hs          |   1 -
 Cabal/Distribution/Simple/InstallDirs.hs           |  22 +-
 Cabal/Distribution/Simple/JHC.hs                   |  26 +-
 Cabal/Distribution/Simple/LHC.hs                   |  39 +-
 Cabal/Distribution/Simple/LocalBuildInfo.hs        |  68 +-
 Cabal/Distribution/Simple/PackageIndex.hs          | 153 ++--
 Cabal/Distribution/Simple/PreProcess.hs            |  30 +-
 Cabal/Distribution/Simple/Program.hs               |   3 -
 Cabal/Distribution/Simple/Program/Builtin.hs       |  13 +-
 Cabal/Distribution/Simple/Program/Db.hs            |  14 +-
 Cabal/Distribution/Simple/Program/Find.hs          |  11 +-
 Cabal/Distribution/Simple/Program/GHC.hs           |  49 +-
 Cabal/Distribution/Simple/Program/HcPkg.hs         |  38 +-
 Cabal/Distribution/Simple/Program/Hpc.hs           |  13 +-
 Cabal/Distribution/Simple/Program/Run.hs           |   7 +-
 Cabal/Distribution/Simple/Program/Script.hs        |   2 -
 Cabal/Distribution/Simple/Program/Strip.hs         |  13 +-
 Cabal/Distribution/Simple/Program/Types.hs         |   6 +-
 Cabal/Distribution/Simple/Register.hs              |  51 +-
 Cabal/Distribution/Simple/Setup.hs                 |  59 +-
 Cabal/Distribution/Simple/SrcDist.hs               |  50 +-
 Cabal/Distribution/Simple/Test.hs                  |  19 +-
 Cabal/Distribution/Simple/Test/ExeV10.hs           |  22 +-
 Cabal/Distribution/Simple/Test/LibV09.hs           |  26 +-
 Cabal/Distribution/Simple/Test/Log.hs              |  22 +-
 Cabal/Distribution/Simple/UHC.hs                   |  15 +-
 Cabal/Distribution/Simple/UserHooks.hs             |  13 +-
 Cabal/Distribution/Simple/Utils.hs                 |  59 +-
 Cabal/Distribution/System.hs                       |   7 +-
 Cabal/Distribution/Text.hs                         |   6 +-
 Cabal/Distribution/Utils/NubList.hs                |   3 +-
 Cabal/Distribution/Verbosity.hs                    |   5 +-
 Cabal/Distribution/Version.hs                      |  56 +-
 Cabal/Language/Haskell/Extension.hs                |  26 +-
 Cabal/Setup.hs                                     |   3 +
 Cabal/changelog                                    |   1 +
 Cabal/doc/installing-packages.markdown             | 135 +++
 Cabal/misc/gen-extra-source-files.sh               |  17 +-
 Cabal/misc/travis-diff-files.sh                    |   3 +-
 .../BuildableField/BuildableField.cabal            |  16 +
 Cabal/tests/PackageTests/BuildableField/Main.hs    |   4 +
 Cabal/tests/PackageTests/DeterministicAr/Check.hs  |   4 +-
 .../DuplicateModuleName/DuplicateModuleName.cabal  |  25 +
 .../PackageTests/DuplicateModuleName/src/Foo.hs    |  12 +
 .../PackageTests/DuplicateModuleName/tests/Foo.hs  |  18 +
 .../PackageTests/DuplicateModuleName/tests2/Foo.hs |  18 +
 Cabal/tests/PackageTests/HaddockNewline/A.hs       |   1 +
 .../tests/PackageTests/HaddockNewline/ChangeLog.md |   5 +
 .../HaddockNewline/HaddockNewline.cabal            |  20 +
 Cabal/tests/PackageTests/HaddockNewline/LICENSE    |  30 +
 Cabal/tests/PackageTests/HaddockNewline/Setup.hs   |   2 +
 Cabal/tests/PackageTests/PackageTester.hs          |   6 +-
 .../PackageTests/TestNameCollision/child/Child.hs  |   2 +
 .../TestNameCollision/child/child.cabal            |  19 +
 .../TestNameCollision/child/tests/Test.hs          |  13 +
 .../TestNameCollision/parent/Parent.hs             |   1 +
 .../TestNameCollision/parent/parent.cabal          |  13 +
 .../PackageTests/TestSuiteTests/ExeV10/Check.hs    |  22 +-
 .../PackageTests/TestSuiteTests/ExeV10/my.cabal    |   6 +
 .../TestSuiteTests/ExeV10/tests/test-Short.hs      |  11 +
 Cabal/tests/PackageTests/Tests.hs                  |  29 +
 Cabal/tests/UnitTests.hs                           |   6 +-
 .../{Test => UnitTests}/Distribution/Version.hs    |  68 +-
 HACKING.md                                         |  11 +-
 .../Distribution/Client/BuildReports/Types.hs      |   8 +-
 .../Distribution/Client/BuildReports/Upload.hs     |  22 +-
 cabal-install/Distribution/Client/ComponentDeps.hs |  20 +-
 cabal-install/Distribution/Client/Config.hs        | 107 ++-
 cabal-install/Distribution/Client/Configure.hs     |  19 +-
 cabal-install/Distribution/Client/Dependency.hs    | 103 ++-
 .../Client/Dependency/Modular/Assignment.hs        |  37 -
 .../Client/Dependency/Modular/Builder.hs           |   5 +
 .../Dependency/Modular/ConfiguredConversion.hs     |  19 +-
 .../Client/Dependency/Modular/Dependency.hs        |  75 +-
 .../Client/Dependency/Modular/Explore.hs           |  31 -
 .../Distribution/Client/Dependency/Modular/Flag.hs |   5 -
 .../Client/Dependency/Modular/IndexConversion.hs   |  52 +-
 .../Distribution/Client/Dependency/Modular/Log.hs  |  21 -
 .../Distribution/Client/Dependency/Modular/PSQ.hs  |  16 +-
 .../Client/Dependency/Modular/Package.hs           |  23 +-
 .../Client/Dependency/Modular/Preference.hs        |  88 +-
 .../Client/Dependency/Modular/Solver.hs            |   4 +-
 .../Distribution/Client/Dependency/Modular/Tree.hs |   8 -
 .../Client/Dependency/Modular/Version.hs           |   5 -
 .../Distribution/Client/Dependency/TopDown.hs      |  28 +-
 .../Distribution/Client/Dependency/Types.hs        |  37 +-
 cabal-install/Distribution/Client/Fetch.hs         |  24 +-
 cabal-install/Distribution/Client/FetchUtils.hs    |  27 +-
 cabal-install/Distribution/Client/Freeze.hs        |  24 +-
 cabal-install/Distribution/Client/Get.hs           |  24 +-
 cabal-install/Distribution/Client/GlobalFlags.hs   | 281 ++++++
 cabal-install/Distribution/Client/HttpUtils.hs     |  23 +-
 cabal-install/Distribution/Client/IndexUtils.hs    | 194 +++--
 cabal-install/Distribution/Client/Init.hs          |  10 +-
 cabal-install/Distribution/Client/Install.hs       | 210 ++---
 cabal-install/Distribution/Client/InstallPlan.hs   | 190 +++--
 .../Distribution/Client/InstallSymlink.hs          |   8 +-
 cabal-install/Distribution/Client/List.hs          |  30 +-
 cabal-install/Distribution/Client/PackageIndex.hs  |   8 +-
 cabal-install/Distribution/Client/PlanIndex.hs     |  91 +-
 cabal-install/Distribution/Client/Sandbox.hs       |   6 +-
 cabal-install/Distribution/Client/Sandbox/Index.hs |  52 +-
 cabal-install/Distribution/Client/Security/HTTP.hs | 174 ++++
 cabal-install/Distribution/Client/Setup.hs         | 139 +--
 cabal-install/Distribution/Client/SetupWrapper.hs  | 140 ++-
 cabal-install/Distribution/Client/Tar.hs           | 950 +--------------------
 cabal-install/Distribution/Client/Targets.hs       |  39 +-
 cabal-install/Distribution/Client/Types.hs         | 134 ++-
 cabal-install/Distribution/Client/Update.hs        |  58 +-
 cabal-install/Distribution/Client/Upload.hs        |  41 +-
 cabal-install/Distribution/Client/Utils.hs         |  43 +-
 cabal-install/Main.hs                              | 151 ++--
 cabal-install/bootstrap.sh                         |  49 +-
 cabal-install/cabal-install.cabal                  |  64 +-
 cabal-install/changelog                            |   8 +
 cabal-install/tests/IntegrationTests.hs            |   2 +
 .../should_run/report_success_removing_source.out  |   2 +-
 .../tests/IntegrationTests/user-config/common.sh   |   9 +
 .../should_fail/doesnt_overwrite_without_f.err     |   1 +
 .../should_fail/doesnt_overwrite_without_f.sh      |   6 +
 .../user-config/should_run/cabal-config            | 185 ++++
 .../user-config/should_run/overwrites_with_f.out   |   2 +
 .../user-config/should_run/overwrites_with_f.sh    |   9 +
 .../user-config/should_run/runs_without_error.out  |   1 +
 .../user-config/should_run/runs_without_error.sh   |   7 +
 .../user-config/should_run/uses_CABAL_CONFIG.out   |   1 +
 .../user-config/should_run/uses_CABAL_CONFIG.sh    |   5 +
 .../Distribution/Client/Dependency/Modular/DSL.hs  | 104 ++-
 .../Client/Dependency/Modular/Solver.hs            | 130 ++-
 .../tests/UnitTests/Distribution/Client/Tar.hs     |  66 +-
 .../UnitTests/Distribution/Client/UserConfig.hs    |  15 +-
 setup-dev.sh                                       |   5 +-
 167 files changed, 3915 insertions(+), 3757 deletions(-)
 delete mode 100644 Cabal/Distribution/Simple/GHC/IPI641.hs
 create mode 100644 Cabal/Distribution/Simple/GHC/IPIConvert.hs
 create mode 100644 Cabal/tests/PackageTests/BuildableField/BuildableField.cabal
 create mode 100644 Cabal/tests/PackageTests/BuildableField/Main.hs
 create mode 100644 Cabal/tests/PackageTests/DuplicateModuleName/DuplicateModuleName.cabal
 create mode 100644 Cabal/tests/PackageTests/DuplicateModuleName/src/Foo.hs
 create mode 100644 Cabal/tests/PackageTests/DuplicateModuleName/tests/Foo.hs
 create mode 100644 Cabal/tests/PackageTests/DuplicateModuleName/tests2/Foo.hs
 create mode 100644 Cabal/tests/PackageTests/HaddockNewline/A.hs
 create mode 100644 Cabal/tests/PackageTests/HaddockNewline/ChangeLog.md
 create mode 100644 Cabal/tests/PackageTests/HaddockNewline/HaddockNewline.cabal
 create mode 100644 Cabal/tests/PackageTests/HaddockNewline/LICENSE
 create mode 100644 Cabal/tests/PackageTests/HaddockNewline/Setup.hs
 create mode 100644 Cabal/tests/PackageTests/TestNameCollision/child/Child.hs
 create mode 100644 Cabal/tests/PackageTests/TestNameCollision/child/child.cabal
 create mode 100644 Cabal/tests/PackageTests/TestNameCollision/child/tests/Test.hs
 create mode 100644 Cabal/tests/PackageTests/TestNameCollision/parent/Parent.hs
 create mode 100644 Cabal/tests/PackageTests/TestNameCollision/parent/parent.cabal
 create mode 100644 Cabal/tests/PackageTests/TestSuiteTests/ExeV10/tests/test-Short.hs
 rename Cabal/tests/{Test => UnitTests}/Distribution/Version.hs (93%)
 create mode 100644 cabal-install/Distribution/Client/GlobalFlags.hs
 create mode 100644 cabal-install/Distribution/Client/Security/HTTP.hs
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/common.sh
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/should_fail/doesnt_overwrite_without_f.err
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/should_fail/doesnt_overwrite_without_f.sh
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/should_run/cabal-config
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/should_run/overwrites_with_f.out
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/should_run/overwrites_with_f.sh
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/should_run/runs_without_error.out
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/should_run/runs_without_error.sh
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/should_run/uses_CABAL_CONFIG.out
 create mode 100644 cabal-install/tests/IntegrationTests/user-config/should_run/uses_CABAL_CONFIG.sh
remote: Counting objects: 41, done.
remote: Compressing objects: 100% (23/23), done.
remote: Total 41 (delta 19), reused 9 (delta 9), pack-reused 8
Unpacking objects: 100% (41/41), done.
From github.com:haskell/cabal
 * branch            master     -> FETCH_HEAD
   dca1c96..407d4c8  master     -> upstream/master
First, rewinding head to replay your work on top of it...
Fast-forwarded master to 407d4c8e267e4468603e551aa4250de0158342a3.
Counting objects: 90, done.
Delta compression using up to 8 threads.
Compressing objects: 100% (39/39), done.
Writing objects: 100% (41/41), 6.76 KiB | 0 bytes/s, done.
Total 41 (delta 25), reused 0 (delta 0)
To git@github.com:Heather/cabal.git
   09a7192..407d4c8  master -> master
```

[Twitter]: http://mxtoolbox.com/Public/images/twitter-icon.png
