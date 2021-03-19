#!/bin/bash

score=0
max=0

echo "Positive tests (files in tests/good/)"

for f in tests/good/*.jqer; do
    max=`expr $max + 1`;
    echo $f
    expected=tests/good/`basename $f .jqer`.out
    rm -f out
    if ./JQER $f > out; then
      if cmp --quiet out $expected; then
          score=`expr $score + 1`;
      else
          echo "  FAIL: bad result for $f"
      fi
    else
      echo "  FAIL: interpreting $f"
    fi
done
echo

echo "Negative tests (files in tests/bad/)"

for f in tests/bad/*.jqer; do
    max=`expr $max + 1`;
    echo $f
    rm -f out
    if ./JQER $f > out 2>&1; then
	echo "  FAIL: Evaluation of $f should fail"
    else
        if grep -q "^error:" out; then
	    score=`expr $score + 1`;
        else
            echo "  FAIL: should display 'error'"
        fi
    fi
done

echo
percent=`expr 100 \* $score / $max`;
echo "Score: $score / $max tests, soit $percent%"
