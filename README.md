# Automaton

[This repository](https://github.com/koba-e964/automaton) contains algorithms for operations of finite/infinite automata written in Haskell.

## Dependencies

* Haskell (GHC 7.8.2)

## How to run
If you run command
```
cabal configure
cabal run
```
, the executable performs syntheses of automata from 3 LTL formulae:
```
G(p -> F q)
(~p) U (X q)
G(F p -> q)
```
(`~` means negation)

## Functions
Currently, this repository includes:

| function | status |
| :-: | :-: |
| NFA -> DFA | ok(tested) |
| AFA -> NFA | ok(tested) |
|synthesis(LTL -> ABA) | not tested |
|ABA -> NBA | not yet implemented |
|complementation of NBA | not yet implemented |


## References
Algorithms in this repository are based on following papers:

Synthesis of ABA: Vardi, M. Y. (1996). An automata-theoretic approach to linear temporal logic. In Logics for concurrency (pp. 238-266). Springer Berlin Heidelberg. 


## Author
Hiroki Kobayashi (silentkiddie-2014@yahoo.co.jp)

If you send bug reports, pull requests and/or suggestions, it will be appreciated.