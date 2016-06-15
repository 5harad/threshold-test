#! /usr/bin/python 
from __future__ import print_function
import sys, argparse, os, pandas, csv
import urllib
import tarfile


  
def download(url, path):
	"""Copy the contents of a file from a given URL to a local file in the directory given by 'path'.
	"""
	webFile = urllib.urlopen(url)
	localFile = open(path+url.split('/')[-1], 'w')
	localFile.write(webFile.read())
	webFile.close()
	localFile.close()


def untar(fname, path=''):
  if (fname.endswith("tar.gz")):
    tar = tarfile.open(fname)
    tar.extractall(path)
    tar.close()
   


if __name__ == '__main__':

  url = 'http://5harad.com/data/NC-stops-2009-2014.tar.gz'
  path = '../data/'
  download(url, path)
  
  fname = 'NC-stops-2009-2014.tar.gz'
  untar(path+fname, path)

 
