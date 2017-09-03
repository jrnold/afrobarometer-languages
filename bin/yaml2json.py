#!/usr/bin/env python
import yaml
import json
import sys

def yaml2json(src, dst):
    with open(src, 'r') as f:
        data = yaml.load(f)
    with open(dst, 'w') as f:
        json.dump(data, f, indent = 2)

def main():
    yaml2json(*sys.argv[1:3])

if __name__ == "__main__":
    main()
