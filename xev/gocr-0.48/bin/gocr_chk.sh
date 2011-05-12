#!/bin/bash
# script to check gocr results of new versions against a testbase
# to ensure that recognition is not worse than a minimum level,
# testbase is a base of (png,pnm,jpg)-image files and a textfile
# with the same name and appended suffix .txt
# containing the desired output, differences will be shown
#
# example:
#  bin/gocr_chk.sh testbase/free/{clean,glued,dusty}/{numbers,text}/
#
# ToDo: output final statistics
# version
#  2009-07-29 numfiles=6 bad=3 good=3 numchars=269 bad=13 tmp09 jocr  5%
#
GOCR=gocr
numfiles=0      # number of checked files
numbad=0        # bad recognitions (ToDo: false positive, false negative)
numgood=0       # good recognitions
numchars=0      # tested chars
badchars=0      # bad chars
dirs=.
numbers=0       # check numbers only?
if test "$1" = "-n"; then numbers=1; shift; fi
if test -d "$1"; then dirs="$*"; fi
if test -x ./gocr; then GOCR=./gocr; fi
for tfile in $(find $dirs -name \*.txt|xargs); do
  ifile=$(echo ${tfile} | sed 's/\.txt//')
  if [ -r $ifile ]; then
    numfiles=$(($numfiles+1))
    x=$(cat ${tfile} | wc -m)
    # echo -ne "test ${ifile}{.txt}  chars=$x\n"
    printf "test %-59s chars= %4d\n" "${ifile}" $x
    numchars=$(($numchars + $x))
    if ${GOCR} ${ifile} | diff -b -B -u ${tfile} -; then
      numgood=$(($numgood+1))
    else
      # count different chars
      x=$(${GOCR} ${ifile} | cmp -l ${tfile} - | wc -l) # charnum ibyte obyte
      badchars=$(($badchars+$x))
      numbad=$(($numbad+1))
    fi
  fi
done
echo -e " : $(date +%F)  numfiles= $numfiles bad= $numbad  numchars= $numchars bad= $badchars  $(($badchars * 100 / $numchars))%"
