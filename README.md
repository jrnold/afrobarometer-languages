The yaml file contains mappings between Afrobarometer languages and WALS languages.
While most Afrobarometer languages were matched to WALS languages by first
matching them to ISO 639-3 languages and then using ISO 639-3 to WALS mappings,
there are many cases in which the mapping is one Afrobarometer to many WALS languages.
The mappings in this data resolve those. All one-to-many mappings were manually checked,
and resolved. There are still one-to-many mappings, but the manual checking pruned some
cases.



# ISO Codes


Output Files
-------------

Afrobarometer_Langs_to_WALS

- question
- id
- name
- wals_code
- wals_name
- num_matches

Afrobarometer_Langs_to_ISO_639_3

- question
- id
- name
- iso_code
- iso_name
- num_matches

Custom ISO codes

----- ----------------- ------
-1    Missing           qna
9995  Other             qot
9999  Don't Know        qdk
9998  Refused to answer qra
      No match          unk
710   Asian/Indian      qai
----- ----------------- ------
