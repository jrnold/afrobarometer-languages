R = Rscript
PYTHON = python3

AFROBAROMETER_MAPPINGS = $(wildcard data-raw/afrobarometer-mappings/r*.yml)
AFROBAROMETER_OTHER_MAPPINGS = data-raw/afrobarometer-mappings/other.yml
AFROBAROMETER_DATA = $(wildcard external/afrobarometer/*.sav)
WALS_DATA = external/wals-language.csv.zip
ISO_DATA = external/iso-639-3_Code_Tables_20170217.zip external/Language_Code_Data_20170221.zip
GLOTTOLOG_DIR = external/glottolog
GLOTTOLOG_FILES = $(wildcard $(GLOTTOLOG_DIR)/*)

OUTPUTS =

all:
	@echo $(OUTPUTS)

download:
	$(R) bin/download.R
.PHONY: download

validate-mappings: $(AFROBAROMETER_MAPPINGS)
	$(PYTHON) src/
.PHONY: validate-mappings


external/glottolog/tree-glottolog.json: bin/glottolog-tree.py external/glottolog/tree-glottolog-newick.txt
	$(PYTHON) $<
OUTPUTS += $(GLOTTOLOG_DIR)/tree-glottolog.json

data/glottolog.csv: bin/glottolog.R external/glottolog/tree-glottolog.json $(GLOTTOLOG_FILES) $(WALS_DATA)
	$(R) $<
OUTPUTS += data/glottolog.csv


data/datapackage.json: bin/yaml2json.py data-raw/datapackage.yml
	$(PYTHON) $^ $@
OUTPUTS += data/datapackage.json


data/afrobarometer_langs.csv: bin/afrobarometer_langs.R  $(AFROBAROMETER_DATA)
	$(R) $<
OUTPUTS += data/afrobarometer_langs.csv

data/afrobarometer_langs_other.csv: bin/afrobarometer_langs_other.R $(AFROBAROMETER_DATA)
	$(R) $<
OUTPUTS += data/afrobarometer_langs_other.csv


data/afrobarometer_to_iso_639_3.csv: bin/afrobarometer_langs_to_iso_639_3.R data/afrobarometer_langs.csv $(AFROBAROMETER_MAPPINGS) $(ISO_DATA)
	$(R) $<
OUTPUTS += data/afrobarometer_to_iso_639_3.csv

data/afrobarometer_other_to_iso_639_3.csv: bin/afrobarometer_other_to_iso_639_3.R data/afrobarometer_langs_other.R $(AFROBAROMETER_OTHER_MAPPINGS) $(ISO_DATA)
	$(R) $<
OUTPUTS += data/afrobarometer_other_to_iso_639_3.csv


data/afrobarometer_to_glottolog.csv: bin/afrobarometer_langs_to_glottolog.R data/afrobarometer_langs.csv $(AFROBAROMETER_MAPPINGS) data/glottolog.csv
	$(R) $<
OUTPUTS += data/afrobarometer_other_to_iso_639_3.csv

data/afrobarometer_other_to_glottolog.csv: bin/afrobarometer_other_to_glottolog.R data/afrobarometer_langs_other.csv $(AFROBAROMETER_OTHER_MAPPINGS) data/glottolog.csv
	$(R) $<
OUTPUTS += data/afrobarometer_other_to_iso_639_3.csv


data/afrobarometer_to_wals.csv: bin/afrobarometer_to_wals.R data/afrobarometer_to_wals.csv $(AFROBAROMETER_MAPPINGS) data/afrobarometer_to_glottolog.csv $(WALS_DATA)
	$(R) $<
OUTPUTS += data/afrobarometer_to_wals.csv

data/afrobarometer_other_to_wals.csv: bin/afrobarometer_other_to_wals.R data/afrobarometer_other_to_wals.csv $(AFROBAROMETER_OTHER_MAPPINGS) data/afrobarometer_other_to_glottolog.csv $(WALS_DATA)
	$(R) $<
OUTPUTS += data/afrobarometer_other_to_wals.csv


data/afrobarometer_respno_to_langs.csv: data/afrobarometer_respno_to_langs.R
	$(R) $<
OUTPUT += data/afrobarometer_respno_to_langs.csv

all: $(OUTPUTS)
