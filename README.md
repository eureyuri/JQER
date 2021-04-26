# JQER
JQER is a Python-like language with static typing. Today, Python is one of the top picks for beginner programmers for how simple the syntax is and its readability. However, we believe that due to the dynamic type system, it also introduces many problems since the programmer will not know if their program will run without errors until they execute it. Therefore, the main goal for JQER is to simplify writing of programs with Python-like syntax and introduce a static typing system.

## Setup
### Running docker image
```bash
$ git clone https://github.com/eureyuri/JQER.git
$ cd JQER
$ docker build -t jqer .
$ docker run -d jqer
$ docker run --rm -it -v `pwd`:/home/jqer -w=/home/jqer jqer
```

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

## Sample Program in JQER
Here is a sample program that will add two numbers and print. To run this file, we need to create a file with a .jqer extension such as add.jqer.
Some things to keep in mind are that a main method is mandatory in JQER and all variables must be declared at the beginning of the method before the value is assigned.
```python
# this is the method for adding 2 numbers
int def add(int a, int b):
  return a + b

int def main():
  int a
  str hello
  a = add(7, 2)
  print(a)
  hello = “Hello, world!”
  print(hello)
```

Output
9
Hello, world!
