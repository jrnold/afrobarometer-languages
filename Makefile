R = Rscript
PYTHON = python3

AFROBAROMETER_DATA = $(wildcard external/afrobarometer/*.sav)

INIT = src/R/init.R

LINGDATA_S3_BUCKET = "s3://jrnold-data/lingdata/"
LINGDATA_DIR = external/lingdata/
WALS_DATA = $(LINGDATA_DIR)/wals.db
ISO_DATA = $(LINGDATA_DIR)/iso_639_3.db
ETHNOLOGUE_DATA = $(LINGDATA_DIR)/ethnologue.db
GLOTTOLOG_DATA = $(LINGDATA_DIR)/glottolog.db
ASJP_DATA = $(LINGDATA_DIR)/asjp.db

OUTPUTS =

all: download data

# Download all files
download: download-afrobarometer download-lingdata
.PHONY: download

download-afrobarometer: bin/download.R
.PHONY: download-afrobarometer

download-lingdata: bin/lingdata.sh
.PHONY: download-lingdata

# All .csv files should be created by a bin/*.R script
# with the same name.
data/%.csv:
	$(R) bin/$(basename $(notdir $@)).R

data/datapackage.json: bin/yaml2json.R data-raw/datapackage.yml
	$(R) $^ $@
data: data/datapackage.json

data/afrobarometer_variables.csv: bin/afrobarometer_variables.R \
	data-raw/afrobarometer_variables.csv \
	$(INIT)
data: data/afrobarometer_variables.csv

data/language_variables.csv: bin/language_variables.R \
	data/afrobarometer_variables.csv \
	$(INIT)
data: data/language_variables.csv

data/countries.csv: bin/countries.R \
	data/afrobarometer_variables.csv \
	data-raw/countries.yml \
	$(AFROBAROMETER_DATA) \
	$(INIT)
data: data/countries.csv

data/language_names.csv: bin/language_names.R \
	data/afrobarometer_variables.csv \
	data/countries.csv \
	data-raw/languages.yml \
	$(GLOTTOLOG_DB) \
	$(INIT)
data: data/language_names.csv

data/languages_respno.csv: bin/languages_respno.R \
	data/afrobarometer_variables.csv \
	data/countries.csv \
	data/language_names.csv \
	$(AFROBAROMETER_DATA) \
	$(INIT)
data: data/languages_respno.csv

data/language_values.csv: bin/language_values.R \
	data/afrobarometer_variables.csv \
	data/countries.csv \
	data/language_names.csv \
	$(AFROBAROMETER_DATA) \
	$(INIT)
data: data/language_values.csv

data/multiple_languages.csv: bin/multiple_languages.R \
	data/countries.csv \
	data/language_names.csv \
	$(AFROBAROMETER_DATA) \
	$(INIT)
data: data/multiple_languages.csv

# --- Validate mappings ---
validate-mappings: bin/validate-yaml.sh $(subst %.schema,%.yml,$(wildcard data-raw/*.schema))
	$<
.PHONY: validate-mappings

validate-datapackage:
	goodtables --row-limit 10000000 --error-limit 100 --table-limit 100 data/datapackage.json
.PHONY: validate-datapackage
