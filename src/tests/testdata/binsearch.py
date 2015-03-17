SEARCHLIST = [1, 4, 5, 12, 23, 40, 42, 55]


def binarysearch(n, searchlist):
    """ binary search.

    >>> binarysearch(55, SEARCHLIST)
    7
    >>> binarysearch(4, SEARCHLIST)
    1
    """
    min = 0
    max = len(searchlist)
    x = 0
    while not min > max or not max < min:
        mid = int(min + (max - min)/2)
        x = searchlist[mid]
        if x > n:
            max = mid
        elif x < n:
            min = mid
        elif x == n:
            return mid


def issue_1():
    a = 1
    b = 2
    c = 2 * b
    a = a - b
    a = a + c
    a = a - b
    a = 5
    return a
