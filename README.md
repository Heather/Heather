Sharingan (mirana recode)
========================

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
            when (sync == "" || isInfixOf sync (location repo))
                $  liftA2 (printf "%s <> %s\n") location branch repo
                >> liftA3 rebasefork location branch upstream repo
```

![](http://fc01.deviantart.net/fs70/f/2011/188/d/2/ember_mangekyou_sharingan_by_jinseiasakura-d3lcdmk.png)

config example:

```yaml
-   location: '/home/repo1'
    branch: 'master'
    upstream: 'upstream master'
-   location: '/home/repo2'
    branch: 'master'
    upstream: 'upstream master'
```
