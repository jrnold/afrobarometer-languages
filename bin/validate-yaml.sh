#!/bin/bash
# Validate all YAML and JSON data files
#
# This script requires npm and the following npm packages
#
# - yamljs: https://www.npmjs.com/package/yamljs
# - ajv-cli: https://github.com/jessedc/ajv-cli
#
YAMLFILES="languages countries"

tojson () {
  yamlfile=data-raw/${1}.yml
  jsonfile=data-raw/${1}.json
  echo "Converting ${yamlfile} to ${jsonfile}"
  yaml2json $yamlfile > $jsonfile
}

validate() {
  schema=data-raw/${1}.schema
  jsonfile=data-raw/${1}.json
  echo "Validating ${jsonfile} with ${schema}"
  ajv -s $schema -d $jsonfile
}

for f in $YAMLFILES
do
  tojson $f
  validate $f
done

