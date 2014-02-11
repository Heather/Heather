Sharingan
=========

[![Build Status](https://travis-ci.org/Heather/Sharingan.png?branch=master)](https://travis-ci.org/Heather/Sharingan)

```haskell
go force sync = (</> "sharingan.yml")
 <$> takeDirectory
 <$> getExecutablePath >>= \ymlx ->
    doesFileExist ymlx >>= (flip when $ lyricsBracket $ do
        ymlData <- BS.readFile ymlx
        let ymlDecode = Data.Yaml.decode ymlData :: Maybe [Repository]
            repoData  = fromJust ymlDecode
        forM_ repoData $ \repo ->
            let loc = (location repo)
            in when (sync == "" || isInfixOf sync loc)
                $ forM_ (branches repo) $ \branch ->
                    printf "%s <> %s\n" loc branch
                    >> rebasefork loc branch (upstream repo)
                    >>= ( flip when 
                        $ let sharingan = (loc </> ".sharingan.yml")
                          in doesFileExist sharingan >>= ( flip when 
                            $ do ymlDataS <- BS.readFile sharingan
                                 let ymlDecodeS = Data.Yaml.decode ymlDataS :: Maybe Sharingan
                                     repoDataS  = fromJust ymlDecodeS
                                 forM_ (script repoDataS) $ exc loc ))
```

![](http://fc01.deviantart.net/fs70/f/2011/188/d/2/ember_mangekyou_sharingan_by_jinseiasakura-d3lcdmk.png)

config example:

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
-   location: D:\Heather\Contrib\P\UnionArgParser
    branches:
        - master
    upstream: upstream master
-   location: D:\TFS\winforms-modernui
    branches:
        - master
    upstream: upstream master
-   location: D:\TFS\quartznet
    branches:
        - master
    upstream: upstream master
-   location: D:\TFS\log4net
    branches: 
        - trunk
        - heather
    upstream: upstream trunk
-   location: D:\TFS\Fuchu
    branches: 
        - master
    upstream: upstream master
-   location: D:\TFS\cetmodbus
    branches: 
        - master
    upstream: upstream master
-   location: D:\TFS\nant
    branches: 
        - master
    upstream: upstream master
-   location: D:\Heather\Contrib\P\fsharp
    branches: 
        - heather
    upstream: upstream fsharp_31
```
