# Extract the language tree for Ethnologue languages
# from https://www.ethnologue.com/browse/families

import json
import re
import sys

import requests
import lxml.html
from lxml import etree

def get_family(path):
    print("getting " + path)
    r = requests.get("https://www.ethnologue.com" + path)
    html = lxml.html.fromstring(r.text, "lxml")
    return path, parse_family(html)

def parse_family(html):
    ethn_tree = html.cssselect("div.view-family.view-id-family.ethn-tree")[0]
    divs = ethn_tree.findall("div")
    # flat tree
    if len(divs) == 1:
        return parse_family1(divs[0])
    elif len(divs) == 2:
        return parse_family2(divs)
    else:
        print("Unknown number of divs in ", path)

def parse_subgroup_text(x):
    return dict(zip(('name', 'number'),
                      re.match(r"(.*) \((\d+)\)$", x).groups()))

def parse_lang(el):
    data = {}
    path = el.xpath('.//a[contains(@href, "/language/")]')[0].get("href")
    data["name"] = el.xpath(".//span[contains(@class, 'field-content')]/text()[1]")[0].strip()
    data["iso_code"] = el.xpath(".//a[contains(@href, '/language/')]/text()")[0][1:-1]
    data["country"] = {
      "path": el.xpath(".//a[contains(@href, '/country/')]")[0].get("href"),
      "name": el.xpath(".//a[contains(@href, '/country/')]/text()")[0]
    }
    return (path, data)

def parse_family1(el):
    """ Parse language family with no-subgroups """
    langs = el.cssselect("li.lang-indent")
    data = parse_subgroup_text(el.cssselect("div.views-field-name-1 > span.field-content")[0].text)
    data['languages'] = dict(parse_lang(x) for x in langs)
    return data

def parse_family2(divs):
    """ Parse language family with subgroups """
    data = parse_subgroup_text(divs[0].
      cssselect("div.views-field-name-1 > span.field-content")[0].text)
    item_list = divs[1].xpath("div[@class='item-list']/ul")[0]
    data['subgroups'] = parse_item_list(item_list)
    return data

def parse_item_list(el):
    return dict(parse_item(li) for li in el.findall("li"))

def parse_item(el):
    path = el.find("a").get("href")
    data = parse_subgroup_text(el.find("a").text.strip())
    print(path)
    langs = el.xpath("div[contains(@class, 'view-id-language')]//li[contains(@class, 'lang-indent')]")
    if len(langs):
        data['languages'] = dict(parse_lang(x) for x in langs)
        print(str(len(data['languages'])) + " languages")
    else:
        data['languages'] = []
    item_list = el.xpath("div[@class='item-list']")
    if len(item_list):
        data['subgroups'] = dict(parse_item_list(item_list[0].find("ul")))
    else:
        data['subgroups'] = []
    return (path, data)

def get_all_families():
    r = requests.get("https://www.ethnologue.com/browse/families")
    html = lxml.html.fromstring(r.text)
    families = [a.get("href") for a in html.xpath("//a[contains(@href, '/subgroups/')]")]
    return dict([get_family(x) for x in families])

def download(out):
    with open(out, 'w') as f:
        json.dump(get_all_families(), f)

def main():
    out = sys.argv[1]
    download(out)

if __name__ == "__main__":
    main()
