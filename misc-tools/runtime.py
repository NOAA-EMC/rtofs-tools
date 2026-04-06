#!/usr/bin/env python3
import os
import sys
import re
import datetime
import time

if len(sys.argv) == 1:
   print ('Usage:  ', sys.argv[0] ,'<filename>')
   exit()

if not os.path.isfile(sys.argv[1]):
   print ('File ',sys.argv[1], ' not found')
   exit()

found = 0
with open((sys.argv[1]),"r") as f:
   for line in f:
      if re.search(" mtime = ",line):
         mtime = line
         found = found + 1
      if re.search(" stime = ",line):
         stime = line
         found = found + 1

if found != 2:
   print ('Error: string not found in',sys.argv[1])
   exit()

ssplit = stime.split()
syr = int(ssplit[6])
smo = time.strptime(ssplit[3], '%b').tm_mon
sdy = int(ssplit[4])
shms = ssplit[5].split(":")
shr = int(shms[0])
smi = int(shms[1])
sse = int(shms[2])

msplit = mtime.split()
myr = int(msplit[6])
mmo = time.strptime(msplit[3], '%b').tm_mon
mdy = int(msplit[4])
mhms = msplit[5].split(":")
mhr = int(mhms[0])
mmi = int(mhms[1])
mse = int(mhms[2])

start = datetime.datetime(syr, smo, sdy, shr, smi, sse)
ended = datetime.datetime(myr, mmo, mdy, mhr, mmi, mse)

runtime = ended - start
print ('runtime: ', start, ended, runtime, runtime.total_seconds() / 60)

