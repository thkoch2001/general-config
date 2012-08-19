#! /usr/bin/python

import argparse
import sys
from pgmagick import Image, Blob, Geometry

parser = argparse.ArgumentParser(description='Process some integers.')
parser.add_argument('infiles', nargs='*', type=argparse.FileType('r'), default=sys.stdin)
parser.add_argument('-o', default="split%d.pnm")
args = parser.parse_args()

number = 0
outpattern = args.o

for file in args.infiles:
  blob = Blob(file.read())
  file.close()

  # upper half
  im = Image(blob)
  width = im.columns()
  height = im.rows() // 2
  geo = Geometry( '%dx%d' % (width,height) )
  im.crop(geo)
  im.write(outpattern % number)

  # lower half
  im = Image(blob)
  width = im.columns()
  height = im.rows() // 2
  geo = Geometry( '%dx%d+0+%d' % (width,height,height) )
  im.crop(geo)
  im.rotate(180)
  im.write(outpattern % (number + 1))

  number += 2

