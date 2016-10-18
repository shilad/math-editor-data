#!/usr/bin/python -O
#
# Uses the raw data to create the dataset.
#

import codecs
import json
import unicodecsv

js = list(unicodecsv.DictReader(open('../finalData.csv', 'r'), encoding='utf-8'))
data = json.dumps(js)

f = codecs.open('editors.js', 'w', encoding='utf-8')
f.write(u'var editors = ' + data)
f.close()

#print '\n'.join([r['publisher'] for r in js])
