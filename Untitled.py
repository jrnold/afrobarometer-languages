from ruamel.yaml import YAML

yaml = YAML(typ='safe')
yaml.default_flow_style = False

with open("data-raw/languages.yml", "r") as fp:
  langs = yaml.load(fp)

for lang in langs:
  if isinstance(lang['name'], str):
    lang['name'] = [lang['name']]
  try:
    lang['note'] = lang['note'].strip()
  except KeyError:
    pass
  try:
    lang['links'] =  [{'iso_639_3': lang['iso_639_3'],
                     'glottocode': lang['glottocode'],
                     'wals': lang.get('wals')}]
  except KeyError:
    print(lang)



with open("data-raw/multiple-languages.yml", "r") as fp:
  multi_langs = yaml.load(fp)


with open("languages-new.yml", "w") as fp:
  yaml.dump(langs, fp)

