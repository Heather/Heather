Heather (mirana recode)
=======================

[![Build Status](https://travis-ci.org/Heather/Heather.png?branch=master)](https://travis-ci.org/Heather/Heather)

```haskell
go pl force = (</> "sync.yml")
 <$> takeDirectory 
 <$> getExecutablePath >>= \ymlx ->
    doesFileExist ymlx >>= (flip when
        $ do ymlData <- BS.readFile ymlx
             let ymlDecode = Data.Yaml.decode ymlData :: Maybe [Repository]
                 repoData  = fromJust ymlDecode
             forM_ repoData $ \repo -> do
                print repo
                liftA2 rebasefork location branch repo)
```

config example:

```yaml
-   location: '/home/repo1'
    branch: 'master'
    upstream: 'upstream master'
-   location: '/home/repo2'
    branch: 'master'
    upstream: 'upstream master'
```
