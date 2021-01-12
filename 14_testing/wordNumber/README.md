- get rid of no cradle found warning:
  - stack install implicit-hie (get gen-hie)
  - gen-hie > hie.yaml (generate implicit cradle)

- commands to run tests:
  - stack build --test (runs tests & shows results)
  - stack ghci wordNumber:test:wordnumber-test (loads test file)

- in .cabal, under test suite:
  - ghci-options: -main-is <name_of_test_module>
  - prevents the following error:
  ```
  <no location info>: error:
      output was redirected with -o, but no output will be generated
  because there is no Main module.
  ```
  - see https://stackoverflow.com/questions/32465974/haskell-stack-not-building-test-executable
