#!/bin/bash
DIR=external/lingdata
S3_BUCKET=s3://jrnold-data/lingdata/
aws s3 sync $S3_BUCKET $DIR

for dump in ${DIR}/*.sql.gz
do
  dbname=${dump%%.sql.gz}
  if [[ ! -f ${dbname}.db ]]
  then
    gunzip -c ${dbname}.sql.gz | sqlite3 ${dbname}.db
  fi
done
