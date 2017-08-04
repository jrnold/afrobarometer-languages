#!/bin/bash

# exit if any command fails
set -e

run_r() {
  echo "Running $1"
  Rscript $1
}

run_r src/download.R &
wait

run_r src/afrobarometer_lang_variables.R &
run_r src/afrobarometer_country_variables.R &
run_r src/afrobarometer_respno_variables.R &
wait

run_r src/afrobarometer_langs.R &
run_r src/afrobarometer_langs_other.R &
run_r src/iso_to_wals.R &
run_r src/wals_dist.R &
wait

run_r src/afrobarometer_to_iso_639_3.R &
run_r src/afrobarometer_other_to_iso_639_3.R &
wait

run_r src/afrobarometer_to_wals.R &
run_r src/afrobarometer_other_to_wals.R &
run_r src/afrobarometer_to_glottolog.R &
run_r src/afrobarometer_other_to_glottolog.R &
wait

run_r src/afrobarometer_respno_to_langs.R
wait

python src/yaml2json.py data-raw/datapackage.yml data/datapackage.json
