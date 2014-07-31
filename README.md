Sharingan
=========

[![Build Status](https://travis-ci.org/Heather/Sharingan.png?branch=master)](https://travis-ci.org/Heather/Sharingan)

```haskell
go :: Bool -> [String] -> Bool -> Bool -> Bool -> Maybe String -> Maybe String -> IO()
go fast nonops unsafe intera force syn _ =
  withConfig $ \ymlx ->                           
    let ymlprocess = ifSo $ despair $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        forM_ rsdata $ \repo ->
            let loc = location repo
                sync = case syn of
                            Nothing -> if (length nonops) > 0 then Just $ nonops !! 0
                                                              else Nothing
                            Just _  -> syn
            in when (case sync of
                            Just snc -> isInfixOf snc loc
                            Nothing  -> case (enabled repo) of
                                                Just er -> er
                                                Nothing -> True)
                $ let up  = splitOn " " $ upstream repo
                      br  = branches repo
                      ps  = post_rebuild repo
                      u b = do printf " - %s : %s\n" loc b
                               rebasefork loc b up unsafe $ if (length up) > 1
                                                                then up !! 1 `elem` br
                                                                else False
                      eye r = when ((r || force) && (not fast))
                                $ do let shx = loc </> ".sharingan.yml"
                                     doesFileExist shx >>= sharingan intera shx loc
                                     when (isJust ps) $ forM_ (fromJust ps) $ \psc ->
                                                            let pshx = psc </> ".sharingan.yml"
                                                            in doesFileExist pshx
                                                                >>= sharingan intera pshx psc
                  in do forM_ (tails br)
                         $ \case x:[] -> u x >>= eye
                                 x:xs -> u x >>= (\_ -> return ())
                                 []   -> return ()
                        putStrLn <| replicate 89 '_'
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
- post_rebuild: null
  branches:
  - master
  - heather
  location: D:\Heather\Contrib\P\rust
  upstream: upstream master
- post_rebuild: null
  branches:
  - master
  location: D:\Heather\clay
  upstream: upstream master
- post_rebuild: null
  branches:
  - master
  - heather
  location: D:\Heather\FSharpPlus
  upstream: upstream master
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
Sharingan 0.0.8 "mingw32"
Usage: sharingan [optional things]
  -v          --version         Display Version
  -h          --help            Display Help
              --make            Create .sharingan.yml template
              --config          Edit .sharingan.yml config file
  -l[STRING]  --list[=STRING]   List repositories
  -a STRING   --add=STRING      Add repository
  -d STRING   --delete=STRING   Delete repository / repositories
              --enable=STRING   Enable repository / repositories
              --disable=STRING  Disable repository / repositories
  -j STRING   --jobs=STRING     Maximum parallel jobs
  -s STRING   --sync=STRING     sync single repository
  -u          --unsafe          do not process reset before sync
  -q          --quick           quick sync, don't process .sharingan.yml files
  -f          --force           force process .sharingan.yml files
  -i          --interactive     trying guess what to do for each repository
  -D          --depot           Get Google depot tools with git and python
```