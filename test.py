# First goal
# print("Hello")

# Second goal
string def make(int n):
    if n == 0:
        return ""
    else:
        return "a" + make(n-1)

print(make(5))

# print(True > False)
# print(1 < 2)
# print(1 == 1)
# print([] < [1])
# print([1] < [1,1,0])
# print([2] > [1,1,0])
# print("a" == "a")
# print("bc" < "z")
# print("bc" < "bca")
# print("bc" > "azzz")
# print([] != [])
