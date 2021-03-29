The JQER Compiler:

------------------------------
To run docker image:

docker run --rm -it -v `pwd`:/home/jqer -w=/home/jqer jqer

------------------------------
To run and test:

$ make

$ ./testall.sh
test-arith1...OK
test-arith2...OK
test-arith3...OK
test-fib...OK
...
fail-while1...OK
fail-while2...OK
