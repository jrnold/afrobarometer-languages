R = Rscript
PYTHON = python3

AFROBAROMETER_MAPPINGS = $(wildcard data-raw/afrobarometer-mappings/r*.yml)
AFROBAROMETER_OTHER_MAPPINGS = data-raw/afrobarometer-mappings/other.yml
AFROBAROMETER_DATA = $(wildcard external/afrobarometer/*.sav)

LINGDATA_S3_BUCKET = "s3://jrnold-data/lingdata/"
LINGDATA_DIR = external/lingdata/
WALS_DATA = $(LINGDATA_DIR)/wals.db
ISO_DATA = $(LINGDATA_DIR)/iso_639_3.db
ETHNOLOGUE_DATA = $(LINGDATA_DIR)/ethnologue.db
GLOTTOLOG_DATA = $(LINGDATA_DIR)/glottolog.db
ASJP_DATA = $(LINGDATA_DIR)/asjp.db

OUTPUTS =

all: download lingdata data

# Download all
download: bin/download.R
	bin/lingdata.sh
	$(R) $<
.PHONY: download


# Linguistic data
lingdata: $(WALS_DATA) $(ISO_DATA) $(ETHNOLOGUE_DATA) $(GLOTTOLOG_DATA) $(ASJP_DATA)
.PHONY: lingdata


# Rule that builds the lingdata databases from their dumps
%.db: %.sql.gz
	-rm -f $@
	gunzip -c $< | sqlite3 $@


data/datapackage.json: bin/yaml2json.py data-raw/datapackage.yml
	$(PYTHON) $^ $@
OUTPUTS += data/datapackage.json


data/afrobarometer_lang_variables.csv: bin/afrobarometer_lang_variables.R
	$(R) $<
data/afrobarometer_lang_variables.csv: 	data-raw/afrobarometer_lang_variables.csv
OUTPUTS += data/afrobarometer_lang_variables.csv


data/afrobarometer_langs.csv: bin/afrobarometer_langs.R
	$(R) $<
data/afrobarometer_langs.csv:	data-raw/misc.yml \
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
	$(GLOTTOLOG_DATA) \
	data-raw/misc.yml
OUTPUTS += data/afrobarometer_other_to_iso_639_3.csv


data/afrobarometer_other_to_glottolog.csv: bin/afrobarometer_other_to_glottolog.R
	$(R) $<
data/afrobarometer_other_to_glottolog.csv: data-raw/misc.yml \
	data/afrobarometer_langs_other.csv \
	$(AFROBAROMETER_OTHER_MAPPINGS) \
	$(GLOTTOLOG_DATA)
OUTPUTS += data/afrobarometer_other_to_iso_639_3.csv


data/afrobarometer_to_wals.csv: bin/afrobarometer_to_wals.R
	$(R) $<
data/afrobarometer_to_wals.csv: data-raw/wals-updates.csv \
	data-raw/misc.yml \
	$(AFROBAROMETER_MAPPINGS) \
	data/afrobarometer_langs.csv \
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
data/afrobarometer_respno_to_langs.csv: $(AFROBAROMETER) \
	data/afrobarometer_langs.csv \
	data/afrobarometer_langs_other.csv \
	data/afrobarometer_to_glottolog.csv \
	data/afrobarometer_other_to_glottolog.csv \
	data/afrobarometer_to_iso_639_3.csv \
	data/afrobarometer_other_to_iso_639_3.csv \
	data/afrobarometer_to_wals.csv \
	data/afrobarometer_other_to_wals.csv
OUTPUTS += data/afrobarometer_respno_to_langs.csv


data/afrobarometer_lang_dists.csv.gz: data/afrobarometer_lang_dists.R
	$(R) $<

# Put this at the end
data: $(OUTPUTS)

# --- Validate mappings ---
validate-mappings: bin/validate_mappings.py
	$(PYTHON) $<
validate-mappings: $(AFROBAROMETER_MAPPINGS)
.PHONY: validate-mappings
