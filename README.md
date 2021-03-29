# The JQER Compiler

## Setup
### Running docker image
`docker run --rm -it -v `pwd`:/home/jqer -w=/home/jqer jqer`

## Running
`$ make`

## Testing
```
$ ./testall.sh
test-arith1...OK
test-arith2...OK
test-arith3...OK
test-fib...OK
...
fail-while1...OK
fail-while2...OK
```