# Afrobarometer Mappings

This folder contains the manual mappings between the Afrobarometer languages and Glottocode, ISO 639-3, and
WALS language identifiers.
These mappings are manually curated.
Each language is manually mapped to a Glottolog languoid and one or more ISO 639-3 languages, and, optionally, one or more WALS codes.

The files named `r[1-6].yml` provide mappings between the categorical responses to the
The file `other.yml` provides mappings for any free-text
responses associated with an "OTHER" response to one of the questions.

The `*.schema` files are [JSON Schema](http://json-schema.org/) used to validate the yml mappings.  These also serve as documentation for
the formats of these files.
`r.schema` validates the `r*.yml` files, while `other.schema` validates `other.yml`.
Run `src/validate.py` to use these schema to validate the `yml` files.
