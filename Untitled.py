from ruamel.yaml import YAML

yaml = YAML(typ='safe')
yaml.default_flow_style = False

with open("data-raw/languages.yml", "r") as fp:
  langs = yaml.load(fp)

newlangs = []
for lang in langs:
  if isinstance(lang['name'], str):
    lang['name'] = [lang['name']]
  try:
    lang['note'] = lang['note'].strip()
  except KeyError:
    pass
  if not lang.get('multi'):
    try:
      del lang['multi']
    except KeyError:
      pass
    newlangs.append(lang)

with open("data-raw/multiple-languages.yml", "r") as fp:
  multi_langs = yaml.load(fp)

new_multi = {x['from']: x['to'] for x in multi_langs}

with open("multiple-languages-new.yml", "w") as fp:
  new_multi = yaml.dump(new_multi, fp)



with open("languages-new.yml", "w") as fp:
  yaml.dump(langs, fp)

