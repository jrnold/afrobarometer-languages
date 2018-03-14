#!/bin/bash
DIR=external/lingdata
S3_BUCKET=s3://jrnold-data/lingdata/
aws s3 sync $S3_BUCKET $DIR
