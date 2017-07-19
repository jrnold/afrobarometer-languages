# Afrobarometer Language Mappings

This contains data on mappings between languages used in the [Afrobarometer](http://www.afrobarometer.org/) survey and common language databases: [ISO 639-3](http://www-01.sil.org/iso639-3/)/[Ethnologue](https://www.ethnologue.com/) and [WALS](http://wals.info/languoid).

Currently only languages for Afrobarometer Round 6 (2016) have been mapped.

# Data

The data consist of several comma-separated tables in the `data` directory.

## Afrobarometer Languages

```
afrobarometer_langs.csv
```

This table contains the questions in which languages appear in the Afrobarometer, and the values and labels used.

|name        |value                                                                                                                                                                  |
|:-----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|lang_id     |Language number, as in the Afrobarometer documentation                                                                                                                 |
|question    |Question number in Afrobarometer                                                                                                                                       |
|lang_name   |Language name, as in the Afrobarometer documentation                                                                                                                   |
|countries   |Countries in which the language appears on the questionnaire.                                                                                                          |
|languages   |Distinct language names. This variable splits `lang_name` when there are multiple languages listed.                                                                    |
|is_language |Does the value refer to a languageas opposed to values like "Other", "No response"                            

# ISO Codes

This table contains the questions in which languages appear in the Afrobarometer, and the values and labels used.

Since the definitions of languages can be ambiguous, the relationship between Afrobarometer and ISO 639-3 languages is potentially many to many. A single Afrobarometer language can map to many ISO 639-3 languages, and single ISO 639-3 language can map to many Afrobarometer languages.

This uses the following versions of the ISO 639-3 and Ethnologue:

- Ethnologue [20160222](https://www.ethnologue.com/codes/Language_Code_Data_20160222.zip)
- ISO 639-3 [20160525](ISO_639_3_URL <- paste0("http://www-01.sil.org/iso639-3/iso-639-3_Code_Tables_20160525.zip"))

|variable         |description                                        |
|:------------|:--------------------------------------------|
|question     |Afrobarometer question number                |
|lang_id      |Afrobarometer language id (variable value)   |
|lang_name    |Afrobarometer language name (variable label) |
|iso_639_3    |ISO-639-3 language code                      |
|iso_ref_name |ISO 639-3 language reference name            |
|iso_scope    |I = individual language, M = macrolanguage   |

The following custom ISO codes are used to indicate missing and other matches:

Custom ISO codes

----- ----------------- ------
-1    Missing           qna
9995  Other             qot
9999  Don't Know        qdk
9998  Refused to answer qra
      No match          unk
710   Asian/Indian      qai
----- ----------------- ------

# WALS

This table provides mappings between Afrobarometer languages and the World Atlas of Language Structures (WALS) database.
As with the ISO 639-3 mappings, the relationship between Afrobarometer and WALS languages is many-to-many.

|variable        |description                                                                                                                 |
|:------------|:---------------------------------------------------------------------------------------------------------------------|
|question     |Afrobarometer question nubmer                                                                                         |
|lang_id      |Afrobarometer language value                                                                                          |
|lang_name    |Afrobarometer language name (label)                                                                                   |
|wals_code    |WALS language code                                                                                                    |
|wals_name    |WALS language name                                                                                                    |
|auto         |If false, manually matched. If true, the closest WALS language to ISO languages matched to the Afrobarometer language |
|distance     |The Ethnologue tree distance of the ISO-WALS language used if an automatic match is used.                             |
|latitude     |latitude of the WALS language                                                                                         |
|longitude    |longitude of the WALS language                                                                                        |
|genus        |WALS language genus                                                                                                   |
|family       |WALS language family                                                                                                  |
|countrycodes |Countries in which the WALS language appears, as space separated ISO-3166 alpha-2 codes.
