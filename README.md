HsBible
=======

A bibtex search tool written in haskell. It supports different output formatting styles. However, this is not yet configurable.

Usage
-----

This tool can be used to search bibtex files using regular expressions. Just use `ghc --make Main.hs` to compile the program. The program expects a bibtex file followed by a set of key/pattern pairs, as arguments. For example:
```
$ ./Main references.bib author 'mccarthy' title 'rec.*func.*symb.*expr.*'
@article{rfsetcm1960,
  author = {McCarthy, John},
  title = {Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I},
  journal = {Commun. ACM},
  issue_date = {April 1960},
  volume = {3},
  number = {4},
  month = {apr},
  year = {1960},
  issn = {0001-0782},
  pages = {184--195},
  numpages = {12},
  url = {http://doi.acm.org/10.1145/367177.367199},
  doi = {10.1145/367177.367199},
  acmid = {367199},
  publisher = {ACM},
}
```
