Mini-TT
=======

OCaml reimplementation of the language described in the article [_A simple type-theoretic language: Mini-TT_](https://doi.org/10.1017/CBO9780511770524.007) by Coquand, Yoshiki, Bengt and Makoto. I translated the [original code](http://www.cse.chalmers.se/research/group/logic/Mini-TT/) to OCaml because I tend to understand better when I write and OCaml is easier for me to read.

Implementation
--------------

The main file for the implementation is [lang.ml](src/lang.ml). It uses [normalization by evaluation](https://en.wikipedia.org/wiki/Normalisation_by_evaluation). We thus introduce two kind of terms: _terms_ and _values_, the second being terms in (weak head) normal form.

We have two kind of environments (which both assign value to variables):

- _rho_ which provides a value for free variables,
- _gamma_ which provides a type for free variables.

For simplicity, we (inconsistently) assume _type in type_.
