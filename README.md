# Haskell Concurrency Tests

```bash
# To see runtime system debug statements
cabal run -- +RTS -Ds
```

## Notes

https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Scheduler#Haskellthreads
https://en.wikipedia.org/wiki/Symmetric_multiprocessing

RTS Source Files  
https://github.com/ghc/ghc/tree/master/includes
https://github.com/ghc/ghc/tree/master/rts
https://github.com/ghc/ghc/blob/master/rts/PrimOps.cmm
https://github.com/ghc/ghc/blob/master/rts/RtsAPI.c

TMVar vs MVar
https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Conc.html#v:atomically
http://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM-TMVar.html
https://stackoverflow.com/questions/15439966/when-why-use-an-mvar-over-a-tvar

ForkIO Gotcha
https://stackoverflow.com/questions/15191649/are-thread-pools-needed-for-pure-haskell-code