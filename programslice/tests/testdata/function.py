import itertools


def func1():
    n = 1
    n = func2(n)
    return n


def func2(i):
    return itertools.count(i + 1)


def main():
    def innerfunc(n):
        return n + 1
    foo = 1
    bar = 1
    baz = foo + bar
    print(baz)

if __name__ == "__main__":
    main()
