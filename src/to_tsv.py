#! /usr/bin/python
from __future__ import print_function
import sys, argparse, os, pandas, csv


def convert_to_tsv(orig_fname, output_fname, path=''):
  orig_fname = path + orig_fname
  format_fname = orig_fname[:-4] + '_format.txt'
  if not os.path.isfile(orig_fname):
    sys.exit('Cannot find file: ' + orig_fname)
  if not os.path.isfile(format_fname):
    sys.exit('Cannot find format file: ' + format_fname)
  cols = []
  with open(format_fname, 'r') as format_file:
    # Skip first two lines
    format_file.readline()
    format_file.readline()
    for line in format_file:
      toks = line.split()
      cols.append({'name': toks[6], 'width': int(float(toks[3]))})
  widths = [x['width'] for x in cols]
  names = [x['name'] for x in cols]
  data = pandas.read_fwf(orig_fname, names = names, widths = widths)
  data.to_csv(path+output_fname, sep = '\t', quoting=csv.QUOTE_NONE, index=False)



if __name__ == '__main__':

  path = '../data/orig_data/'
  
  convert_to_tsv(orig_fname='CONTRABAND.txt', output_fname='CONTRABAND.tsv', path=path)
  convert_to_tsv(orig_fname='SEARCH.txt', output_fname='SEARCH.tsv', path=path)
  convert_to_tsv(orig_fname='SEARCH_BASIS.txt', output_fname='SEARCH_BASIS.tsv', path=path)  
  convert_to_tsv(orig_fname='STOP.txt', output_fname='STOP.tsv', path=path)
  convert_to_tsv(orig_fname='PERSON.txt', output_fname='PERSON.tsv', path=path)
