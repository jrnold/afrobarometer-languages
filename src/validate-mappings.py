#!/usr/bin/env python3
""" Validate the mappings in data-raw/afrobarometer/*.yml

Validate the structure of the yml files against a JSON schema.

"""
import yaml
import json
import jsonshema

def main():
  with open("mappings.schema", 'r') as f:
    schema = json.load(f)
  for r in range(1, 7):
      filename = "afrobarometer-mappings/r%d.yml" % r
      print("Validating %s" % filename)
      with open(filename, 'r') as f:
          data = yaml.load(f)
      jsonschema.validate(data, schema)

if __name__ == "__main__":
  main()
