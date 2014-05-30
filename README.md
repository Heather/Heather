Sharingan
=========

[![Build Status](https://travis-ci.org/Heather/Sharingan.png?branch=master)](https://travis-ci.org/Heather/Sharingan)

```haskell
go :: Bool -> Bool -> String -> String -> IO()
go fast intera sync _ =
  withConfig $ \ymlx ->                           
    let ymlprocess = ifSo $ lyricsBracket $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        forM_ rsdata $ \repo ->
            let loc = location repo
            in when (sync == "" || isInfixOf sync loc)
                $ forM_ (branches repo) $ \branch ->
                    printf " * %s <> %s\n" loc branch
                    >> let eye = ifSo 
                            $ when (not fast)
                            $ let shx = loc </> ".sharingan.yml"
                              in doesFileExist shx >>= sharingan intera shx loc
                        in rebasefork loc branch <| upstream repo >>= eye
                    >>  putStrLn <| replicate 89 '_'
    in doesFileExist ymlx >>= ymlprocess
```

![](http://fc01.deviantart.net/fs70/f/2011/188/d/2/ember_mangekyou_sharingan_by_jinseiasakura-d3lcdmk.png)

per-repository config example (`.sharingan.yml`):
```yaml
script:
  - make -f Makefile-x
```

config example (`sharingan.yml`):

```yaml
-   location: D:\Heather\Contrib\P\coreutils
    branches: 
        - master
    upstream: upstream master
-   location: D:\Heather\Contrib\P\Nemerle
    branches: 
        - master
        - indent
    upstream: upstream master
-   location: D:\TFS\log4net
    branches: 
        - trunk
        - heather
    upstream: upstream trunk
-   location: D:\Heather\Contrib\P\fsharp
    branches: 
        - heather
    upstream: upstream fsharp_31
```

usage example :

``` shell
D:\Heather\Contrib\P\H>sharingan -s coreu
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
>sharingan --help
Usage: sharingan [optional things]
  -v         --version        Display Version
  -h         --help           Display Help
  -D         --depot          Get Google depot tools with git and python
             --make           Create .sharingan.yml template
  -l         --list           List repositories
  -a STRING  --add=STRING     Add repository
  -d STRING  --delete=STRING  Delete repository
  -g         --gentoo         Synchronize cvs portagee tree Gentoo x86
  -j STRING  --jobs=STRING    Maximum parallel jobs
  -s STRING  --sync=STRING    sync single repository
  -q         --quick          quick sync, don't process .sharingan.yml files
  -f         --force          force process .sharingan.yml files
  -i         --interactive    trying guess what to do for each repository
```