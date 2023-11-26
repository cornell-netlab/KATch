# This script cleans up a bibtex file by removing unused entries and
# standardizing citekeys. It also replaces citekeys in tex files.
# It also removes useless fields from the bibtex entries.
# It also replaces some conference names with their abbreviations.
# USE AT YOUR OWN RISK. ALWAYS CHECK THE OUTPUT MANUALLY.

import bibtexparser
from bibtexparser.bwriter import BibTexWriter
import re
import pathlib
import urllib.request
import json

input_bib = "refs.bib"  # Replace with your bib file path
output_bib = "refs.bib"  # Replace with your desired output file path
texfiles = "pldi24.tex" # TeX files to search for cite keys
delete_unused_citekeys = False # delete citekeys not found in TeX files
standardize_citekeys = True # standardize citekeys to author_conference_year
remove_PACMPL = True # remove PACMPL journal name
check_missing_doi = True
search_dblp_enabled = False

# Load the bib file
with open(input_bib, 'r') as bibfile:
  bib_database = bibtexparser.load(bibfile)

# Regex to find \cite{...} commands
cite_regex = r"\\cite(?:t|p|author|)(?:\[[^]]*\])?{([^}]*)}"
used_citekeys = [cite.strip()
  for file in pathlib.Path('.').glob(texfiles)
  for line in open(file)
  for citelist in re.findall(cite_regex,line)
  for cite in citelist.split(',')]

bib_citekeys = [entry['ID'] for entry in bib_database.entries]

if set(used_citekeys) != set(bib_citekeys):
  print('Citekeys do not match:')
  print("Used in TeX but not in bib:")
  print(set(used_citekeys) - set(bib_citekeys))
  print("Found in bib but not used in TeX:")
  print(set(bib_citekeys) - set(used_citekeys))

if delete_unused_citekeys:
  bib_database.entries = [
    entry for entry in bib_database.entries
    if entry['ID'] in used_citekeys
  ]

remove = [
  'series', 'biburl', 'timestamp', 'bibsource', 'publisher', 'editor',
  'pages', 'keywords', 'numpages', 'articleno', 'volume', 'abstract',
  'isbn', 'issn', 'urn', 'month', 'address', 'location', 'note', 'nourlbecauseofdoi',
]

confs = [
  'ECOOP', 'ESOP', 'APLAS', 'ICFP', 'CONCUR', 'TACAS', 'ICECCS',
  'POPL', 'FOSSACS', 'JFP', 'PLACES', 'MSCS', 'PLDI', 'PPDP', 'TOPLAS', 'LICS', 'STACS', 'AISTATS'
]

replace = {conf: conf for conf in confs}
replace['Haskell'] = 'Haskell Symposium'
replace['Principles and Practice of Declarative Programming'] = 'PPDP'

# Loop over each entry in the bib file
for entry in bib_database.entries:
  if 'author' not in entry and 'editor' in entry:
    entry['author'] = entry['editor']
    del entry['editor']
  # If the entry has a "series" field, remove it
  for rem in remove:
    entry.pop(rem, None)
  if 'doi' in entry and 'url' in entry:
    del entry['url']
  if 'journal' in entry:
    if entry['journal'] == 'Proc. {ACM} Program. Lang.':
      entry['journal'] = 'PACMPL'
    if entry['journal'] == 'PACMPL' and remove_PACMPL:
      entry['journal'] = ''
    if entry['journal'] == 'Log. Methods Comput. Sci.':
      entry['journal'] = 'LMCS'
    if 'J. Funct. Program.' in entry['journal']:
      entry['journal'] = 'JFP'
  if 'booktitle' in entry:
    for rep in replace.keys():
      if rep in entry['booktitle']:
        entry['booktitle'] = replace[rep]
    if len(entry['booktitle']) > 20:
       print(entry['booktitle'])
  if 'author' not in entry:
    print('No author for ' + entry['ID'])
    print("- " + str(entry))
  entry['author'] = ' '.join(entry['author'].split())
  if 'title' in entry:
    entry['title'] = ' '.join(entry['title'].split())
  if 'number' in entry:
    entry['number'] = entry['number'].replace('{', '').replace('}', '')
    # if number is only [0-9]*, delete
    if re.match(r"^[0-9-]*$", entry['number']):
      del entry['number']
  if 'journal' in entry:
    entry['journal'] = entry['journal'].replace('{', '').replace('}', '')

def search_dblp(query):
  """Search DBLP for the cite key, replacing _ with space."""
  query = query.replace("_","%20").replace(" ","%20")
  url = f"https://dblp.org/search/publ/api?q={query}&h=10&format=json"
  result = urllib.request.urlopen(url)
  js = json.loads(result.read().decode('utf-8'))
  try:
    return js['result']['hits']['hit']
  except KeyError:
    return []

def get_bibtex(dblpkey):
  """Given a DBLP key, find the bibtex record.
     Optionally, replace the cite key with replacementkey."""
  url = f"https://dblp.org/rec/{dblpkey}.bib"
  result = urllib.request.urlopen(url)
  bib = result.read().decode('utf-8')
  return bib

def search_bib(query):
  """Find the bibtex record for a search query string.
     Gives an error if none or multiple results exist."""
  papers = search_dblp(query)
  bibs = [get_bibtex(paper['info']['key']) for paper in papers]
  return bibs

# Check for missing doi
if check_missing_doi:
  for entry in bib_database.entries:
    if 'doi' not in entry:
      print("")
      print(f"Missing doi for {entry['ID']}")
      if search_dblp_enabled:
        print(f"Searching DBLP for {entry['title']}")
        bibs = "\n".join(search_bib(entry['title']))
        parsed = bibtexparser.loads(bibs)
        # parsed.entries = [e for e in parsed.entries if e['ENTRYTYPE'] == entry['ENTRYTYPE']]
        if len(parsed.entries) == 1:
          print("Found one result:")
          print("- " + str(parsed.entries[0]))
          if 'doi' not in parsed.entries[0]:
            print("No doi found for this entry.")
            continue
          print("Do you want to replace the entry with this one? [y/n]")
          if input() == 'y':
            entry['doi'] = parsed.entries[0]['doi']
            print("Replaced.")
        elif len(parsed.entries) == 0:
          print("Found no results.")
        else:
          print("Found multiple results:")
          for bib in parsed.entries:
            for rem in remove:
              bib.pop(rem, None)
            print("- " + str(bib))
          print('Closest match:')
          def score(bib):
            return int(bib['year'] == entry['year']) + int(bib['ENTRYTYPE'] == entry['ENTRYTYPE']) + \
                    int("".join(bib['title'].split()) == "".join(entry['title'].split()))
          parsed.entries.sort(key=score, reverse=True)
          print("- " + str(parsed.entries[0]))
          if 'doi' not in parsed.entries[0]:
            print("No doi found for this entry.")
            continue
          print("Do you want to replace the entry with this one? [y/n]")
          if input() == 'y':
            entry['doi'] = parsed.entries[0]['doi']
            print("Replaced.")

old2newkey = {}
newdata = {}
if standardize_citekeys:
  for entry in bib_database.entries:
    # Get author surname
    author_surname = entry['author'].split(' and')[0]
    if ',' in author_surname:
      author_surname = author_surname.split(',')[0]
    else:
      author_surname = author_surname.split(' ')[-1]
    author_surname = author_surname.replace("'", '').replace('{', '').replace('}', '').replace('\\', '').replace('-', '')
    author_surname = author_surname.replace('"', '').replace('ß', 'ss').replace('ö', '')

    # Get year
    year = entry['year']

    # Make citekey {author_surname}{year}
    exists = False
    new_key = f"{author_surname}{year}"
    while new_key in newdata:
      print(f"Duplicate key {new_key} for {entry['ID']} and {newdata[new_key]['ID']} !!!")
      otherdata = newdata[new_key]
      if 'title' in otherdata and 'title' in entry and otherdata['title'].lower() == entry['title'].lower():
        print("Same title, ignoring.")
        exists = True
        break
      new_key = new_key + 'a'
    if new_key != entry['ID']:
      print(f"Standardizing {entry['ID']} to {new_key}")
    old2newkey[entry['ID']] = new_key
    if exists:
      entry['ID'] = "REMOVE"
    else:
      newdata[new_key] = entry.copy()
      entry['ID'] = new_key

  def replace_with_dict(text, dic):
      # Create a regular expression that matches any of the keys in dic
      # Sort keys by length to avoid partial matches
      keys = sorted(dic.keys(), key=len, reverse=True)
      pattern = re.compile("(" + ")|(".join([re.escape(k) for k in keys]) + ")", re.M)

      # Function to look up the match
      def lookup(match):
          return dic[match.group(0)]

      # Use the lookup function as the replacement
      return pattern.sub(lookup, text)

  # Replace citekeys in tex files
  for file in pathlib.Path('.').glob(texfiles):
    with open(file, 'r') as texfile:
      tex = texfile.read()
    replace = lambda match: replace_with_dict(match.group(0), old2newkey)
    tex = re.sub(cite_regex, replace, tex)
    with open(file, 'w') as texfile:
      texfile.write(tex)

# Remove entries with ID "REMOVE"
bib_database.entries = [entry for entry in bib_database.entries if entry['ID'] != "REMOVE"]

# Write the bib file back out
with open(output_bib, 'w') as bibfile:
  writer = BibTexWriter()
  writer.comma_first = False  # place the comma at the end
  bibfile.write(writer.write(bib_database))
