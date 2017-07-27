#!/bin/bash

# exit if any command fails
set -e

run_r() {
  echo "Running $1"
  Rscript $1
}

run_r src/download_afrobarometer.R &
run_r src/download_wals.R &
run_r src/download_iso.R &
run_r src/download_ethnologue.R &
wait

run_r src/afrobarometer_langs.R &
run_r src/afrobarometer_langs_other.R &
run_r src/iso_to_wals.R &
wait

run_r src/afrobarometer_to_iso_639_3.R &
run_r src/afrobarometer_other_to_iso_639_3.R &
wait

run_r src/afrobarometer_to_wals.R &
run_r src/afrobarometer_other_to_wals.R &
wait

python src/yaml2json.py data-raw/datapackage.yml data/datapackage.json