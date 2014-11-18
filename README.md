Sharingan
=========

[![Build Status](https://travis-ci.org/Heather/Sharingan.png?branch=master)](https://travis-ci.org/Heather/Sharingan)

```haskell
synchronize :: CommonOpts -> SyncOpts -> IO()
synchronize o so =
  withDefaultsConfig $ \defx ->
   withConfig $ \ymlx ->                           
    let ymlprocess = ifSo $ despair $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        dfdata <- yDecode defx :: IO Defaults
        forM_ rsdata $ \repo ->
            let loc = location repo
                isenabled = fromMaybe True (enabled repo)
            in when (case syncFilter so of
                            Nothing  -> case syncGroups so of
                                            [] -> isenabled
                                            gx  -> case syncGroup repo of 
                                                        Just gg -> isenabled && (gg `elem` gx)
                                                        Nothing -> False
                            Just snc -> isInfixOf snc loc)
                $ let ups = splitOn " " $ upstream repo
                      cln = fromMaybe False (clean repo)
                      noq = case (quick dfdata) of
                                Just qc -> not qc
                                Nothing -> True
                      u b = do printf " - %s : %s\n" loc b
                               amaterasu (task repo) loc b ups (syncUnsafe so) cln (hash repo) 
                                                $ if (length ups) > 1 then ups !! 1 `elem` (branches repo)
                                                                      else False
                      eye (_, r) = when ((r || syncForce so) && (not $ syncQuick so) && noq)
                                    $ do let shx = loc </> ".sharingan.yml"
                                             ps  = postRebuild repo
                                         doesFileExist shx >>= sharingan (syncInteractive so) shx loc
                                         when (isJust ps) $ forM_ (fromJust ps) $ \psc ->
                                                                let pshx = psc </> ".sharingan.yml"
                                                                in doesFileExist pshx
                                                                    >>= sharingan (syncInteractive so) pshx psc
                  in do forM_ (tails (branches repo))
                         $ \case x:[] -> u x >>= eye -- Tail
                                 x:xs -> u x >>= (\_ -> return ())
                                 []   -> return ()
                        putStrLn <| replicate 89 '_'

    in doesFileExist ymlx >>= ymlprocess
```

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
  depot                    Get Google depot tools with git and python
```