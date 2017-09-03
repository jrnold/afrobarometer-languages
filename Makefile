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

download: bin/download.R
	$(R) $<
.PHONY: download

validate-mappings: bin/validate_mappings.py
	$(PYTHON) $<
validate-mappings: $(AFROBAROMETER_MAPPINGS)
.PHONY: validate-mappings

external/glottolog/tree-glottolog.json: bin/glottolog-tree.py
	$(PYTHON) $<
data/glottolog/tree-glottolog.json: external/glottolog/tree-glottolog-newick.txt
OUTPUTS += $(GLOTTOLOG_DIR)/tree-glottolog.json

data/glottolog.csv: bin/glottolog.R
	$(R) $<
data/glottlog.csv: external/glottolog/tree-glottolog.json \
	$(GLOTTOLOG_FILES) \
	$(WALS_DATA)
OUTPUTS += data/glottolog.csv


data/datapackage.json: bin/yaml2json.py data-raw/datapackage.yml
	$(PYTHON) $^ $@
OUTPUTS += data/datapackage.json


data/afrobarometer_langs.csv: bin/afrobarometer_langs.R
	$(R) $<
data/afrobaroemter_langs.csv:	data-raw/misc.yml \
	$(AFROBAROMETER_DATA)
OUTPUTS += data/afrobarometer_langs.csv

data/afrobarometer_langs_other.csv: bin/afrobarometer_langs_other.R
	$(R) $<
data/afrobarometer_langs_other.csv: data-raw/misc.yml \
	$(AFROBAROMETER_DATA)
OUTPUTS += data/afrobarometer_langs_other.csv


data/afrobarometer_to_iso_639_3.csv: bin/afrobarometer_to_iso_639_3.R
	$(R) $<
data/afrobarometer_to_iso_639_3.csv: data/afrobarometer_langs.csv \
	data-raw/misc.yml \
	$(AFROBAROMETER_MAPPINGS) \
	$(ISO_DATA)
OUTPUTS += data/afrobarometer_to_iso_639_3.csv

data/afrobarometer_other_to_iso_639_3.csv: bin/afrobarometer_other_to_iso_639_3.R
	$(R) $<
data/afrobarometer_other_to_iso_639_3.csv: data/afrobarometer_langs_other.csv \
	$(AFROBAROMETER_OTHER_MAPPINGS) \
	$(ISO_DATA)
OUTPUTS += data/afrobarometer_other_to_iso_639_3.csv


data/afrobarometer_to_glottolog.csv: bin/afrobarometer_to_glottolog.R
	$(R) $<
data/afrobarometer_to_glottolog.csv: data/afrobarometer_langs.csv \
	$(AFROBAROMETER_MAPPINGS) \
	data/glottolog.csv \
	data-raw/misc.yml
OUTPUTS += data/afrobarometer_other_to_iso_639_3.csv

data/afrobarometer_other_to_glottolog.csv: bin/afrobarometer_other_to_glottolog.R
	$(R) $<
data/afrobarometer_other_to_glottolog.csv: data-raw/misc.yml \
	data/afrobarometer_langs_other.csv \
	$(AFROBAROMETER_OTHER_MAPPINGS) \
	data/glottolog.csv
OUTPUTS += data/afrobarometer_other_to_iso_639_3.csv


data/afrobarometer_to_wals.csv: bin/afrobarometer_to_wals.R
	$(R) $<
data/afrobarometer_to_wals.csv: data-raw/wals-updates.csv \
	data-raw/misc.yml \
	data/afrobarometer_langs.csv \
	$(AFROBAROMETER_MAPPINGS) \
	data/afrobarometer_to_glottolog.csv \
	$(WALS_DATA)
OUTPUTS += data/afrobarometer_to_wals.csv

data/afrobarometer_other_to_wals.csv: bin/afrobarometer_other_to_wals.R
	$(R) $<
data/afrobarometer_other_to_wals.csv: data-raw/wals-updates.csv	 \
  data-raw/misc.yml \
	data/afrobarometer_langs_other.csv \
	$(AFROBAROMETER_OTHER_MAPPINGS) \
	data/afrobarometer_other_to_glottolog.csv \
	$(WALS_DATA)
OUTPUTS += data/afrobarometer_other_to_wals.csv


data/afrobarometer_respno_to_langs.csv: bin/afrobarometer_respno_to_langs.R
	$(R) $<
data/afrobarometer_respno_to_langs.csv: $(AFROBAROMETER)
data/afrobarometer_respno_to_langs.csv: data/afrobarometer_langs.csv
data/afrobarometer_respno_to_langs.csv: data/afrobarometer_langs_other.csv
data/afrobarometer_respno_to_langs.csv: data/afrobarometer_to_glottolog.csv
data/afrobarometer_respno_to_langs.csv: data/afrobarometer_other_to_glottolog.csv
data/afrobarometer_respno_to_langs.csv: data/afrobarometer_to_iso_639_3.csv
data/afrobarometer_respno_to_langs.csv: data/afrobarometer_other_to_iso_639_3.csv
data/afrobarometer_respno_to_langs.csv: data/afrobarometer_to_wals.csv
data/afrobarometer_respno_to_langs.csv: data/afrobarometer_other_to_wals.csv
OUTPUT += data/afrobarometer_respno_to_langs.csv

data/afrobarometer_lang_dists.csv.gz: data/afrobarometer_lang_dists.R
	$(R) $<

# Put this at the end
all: $(OUTPUT)
