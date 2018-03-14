# Afrobarometer Language Mappings

This contains data on mappings between languages used in the [Afrobarometer](http://www.afrobarometer.org/) survey and common language databases: [ISO 639-3](http://www-01.sil.org/iso639-3/)/[Ethnologue](https://www.ethnologue.com/), [WALS](http://wals.info/languoid),
and [Glottolog](http://glottolog.org/).

Currently only languages for Afrobarometer Round 6 (2016) have been mapped.

# Data

The data consist of several comma-separated tables in the `data` directory.

# Install and Build

The output data is checked into the repo and contained in `data/`, so there should be no need to rebuild the data to use it.

Install R and python prerequisites:
```console
$ Rscript -e 'devtools::install()'
$ pip install requirements.txt
```

Download data and build the Afrobarometer language data:
```console
make build
```

This will create CSV files in the `data` directory.
There is also a `datapackage.json` file with documentation about the data, following the [datapackage standard](https://frictionlessdata.io/data-packages/).


# Validation and Testing

Validating the YAML data sources requires NodeJS and a few packages.
After installing node, install them with
```console
npm install -g ajv-cli yamljs
```

YAML files are documented and validated against [JSON Schema](http://json-schema.org/).
Each YAML file `data-raw/foo.yml` has an associated JSON schema `data-raw/foo.schema`.

Run the validation,
```console
```
