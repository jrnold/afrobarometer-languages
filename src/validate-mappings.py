#!/usr/bin/env python3
""" Validate the mappings in data-raw/afrobarometer/*.yml

Validate the structure of the yml files against a JSON schema.

"""
import json
import jsonschema

import yaml
try:
    from yaml import CLoader as Loader
except ImportError:
    from yaml import Loader
from yaml.constructor import ConstructorError


def no_duplicates_constructor(loader, node, deep=False):
    """Check for duplicate keys."""

    mapping = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=deep)
        value = loader.construct_object(value_node, deep=deep)
        if key in mapping:
            raise ConstructorError("while constructing a mapping", node.start_mark,
                                   "found duplicate key (%s)" % key, key_node.start_mark)
        mapping[key] = value

    return loader.construct_mapping(node, deep)

yaml.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
                     no_duplicates_constructor)

def load_yaml(filename):
    with open(filename, 'r') as f:
        out = yaml.load(f)
    return out


def main():
    with open("data-raw/mappings.schema", 'r') as f:
        schema = json.load(f)
    for r in range(1, 7):
        filename = "data-raw/afrobarometer-mappings/r%d.yml" % r
        print("Validating %s" % filename)
        try:
            with open(filename, 'r') as f:
                data = yaml.load(f)
        except ConstructorError as e:
            print(e)
            continue
        jsonschema.validate(data, schema)

    with open("data-raw/mappings_other.schema", 'r') as f:
        schema = json.load(f)
    filename = "data-raw/afrobarometer_other_mappings.yml"
    print("Validating %s" % filename)
    try:
        with open(filename, 'r') as f:
            data = yaml.load(f)
    except ConstructorError as e:
        print(e)
    jsonschema.validate(data, schema)




if __name__ == "__main__":
  main()
