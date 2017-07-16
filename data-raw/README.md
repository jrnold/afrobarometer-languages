# afrobarometer-country

The yaml file contains mappings between Afrobarometer languages and WALS languages.
While most Afrobarometer languages were matched to WALS languages by first
matching them to ISO 639-3 languages and then using ISO 639-3 to WALS mappings,
there are many cases in which the mapping is one Afrobarometer to many WALS languages.
The mappings in this data resolve those. All one-to-many mappings were manually checked,
and resolved. There are still one-to-many mappings, but the manual checking pruned some
cases.

# wals-updates.csv

The WALS to ISO codes from WALS are not up to date since there have
been revisions to ISO 369-3 since when those codes were generated.

The data here patches the ISO_codes associated with WALS languages.
These patches are from https://github.com/clld/wals-data/issues/107.

This addresses part of #27
