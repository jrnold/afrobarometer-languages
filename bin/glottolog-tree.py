import newick
import json
import re

INFILE = "external/glottolog/tree-glottolog-newick.txt"
OUTFILE = "external/glottolog/tree-glottolog.json"

def parse_node(x):
    node_pattern = """^'?
        (?P<name> .* ) [ ]
        \[ (?P<glottocode> [a-z0-9]{8} ) \]
        (?: \[ (?P<iso_639_3> [a-z]{3} ) \] ) ?
        (?P<language> -l- ) ?
    '?$"""
    m = re.match(node_pattern, x, re.X)
    if m is not None:
        out = m.groupdict()
        out['language'] = out['language'] is not None
        return out

def walk_tree(x):
    node = parse_node(x.name)
    node['children'] = [walk_tree(n) for n in x.descendants]
    return node

def main():
    with open(INFILE, 'r') as f:
        glottolog_tree = [walk_tree(x) for x in newick.load(f)]

    with open(OUTFILE, 'w') as f:
        json.dump(glottolog_tree, f)

if __name__ == "__main__":
    main()
