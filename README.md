# Afrobarometer Language Mappings

This contains data on mappings between languages used in the [Afrobarometer](http://www.afrobarometer.org/) survey and common language databases: [ISO 639-3](http://www-01.sil.org/iso639-3/)/[Ethnologue](https://www.ethnologue.com/), [WALS](http://wals.info/languoid),
and [Glottolog](http://glottolog.org/).

Currently only languages for Afrobarometer Round 6 (2016) have been mapped.

# Data

The data consist of several comma-separated tables in the `data` directory.

# Build

The output data is checked into the repo and contained in `data/`, so there should be no need to rebuild the data to use it.

Install R and python prerequisites:
```console
$ Rscript -e 'devtools::install()'
$ pip install requirements.txt
```

Re-build the data:
```console
$ build.sh
```
