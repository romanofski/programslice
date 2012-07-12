def main():
    def innerfunc(n):
        return n + 1
    foo = 1
    bar = 1
    baz = foo + bar
    print baz

if __name__ == "__main__":
    main()
