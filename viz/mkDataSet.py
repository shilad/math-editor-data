#!/usr/bin/python -O
#
# Uses the raw data to create the dataset.
#

import codecs
import json
import unicodecsv

publishers = {}
for line in open('../publishers.tsv', 'r'):
    tokens = line.split('\t')
    publishers[tokens[0]] = tokens[1].strip()

reader = unicodecsv.DictReader(open('../finalData.csv', 'r'), encoding='utf-8')
js = []
for row in reader:
    # clean up publisher
    pub = publishers[row['publisher']]

    # clean up name
    fn = row['firstName'].split(' / ')[0]
    ln = row['lastName'].split(' / ')[0]
    if fn and ln:
        name = fn + ' ' + ln
    elif fn:
        name = fn
    elif ln:
        name = ln

    # clean up gender
    if row['gender'] == 'male':
        gender = 'man'
    elif row['gender'] == 'female':
        gender = 'woman'
    else:
        gender = 'unknown'

    # clean up confidence
    conf = max(row['probM'], row['probF'])

    # method for inference
    method = 'name' if row['turkGenderScore'] == 'NA' else 'mturk'
    
    js.append({
        'name' : name,
        'gender' : gender,
        'country' : row['country'],
        'journal' : row['journal'],
        'publisher' : pub,
        'confidence' : conf,
        'method' : method,
    })

data = json.dumps(js)

f = codecs.open('editors.js', 'w', encoding='utf-8')
f.write(u'var editors = ' + data)
f.close()
