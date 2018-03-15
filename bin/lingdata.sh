#!/bin/bash
# Download lingdata datasets (WALS, Glottocode, Ethnologue, ISO 639-3, ASJP)
DIR=external/lingdata
S3_BUCKET=s3://jrnold-data/lingdata/
aws s3 sync --exclude "*" --include "*.db" $S3_BUCKET $DIR
