# git-th

## Overview 

`git-th` provides template-haskell functions which generates splices at runtime to embed version control information about a project. 
For example, one can include the revision in vigor at the time the code was compiled. 

Obviously, it requires `git` to be available on the system's path. If not available, functions
will default to opaque values such as `"unknown revision"`.

## Usage


```hs
{-# LANGUAGE TemplateHaskell #-}

import Data.Git.Revision.TH
  ( gitRevParseHEAD )

main :: IO ()
main = do
  let revHEAD = $(gitRevParseHEAD)
  putStrLn $ "revision: " <> revHEAD
```

```
revision: 8901897a8883285ceebae66aa806e8ecceb12a48
```


<hr/>

<p align="center">
  <a href="../../../CONTRIBUTING.md">:gift: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>
