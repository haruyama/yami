#!/usr/bin/env python
# coding: utf-8

import sys

def levenshtein_distance(src, dest):
    dp = {}

    for i in range(len(src) + 1):
        dp[(i, 0)] = i

    for j in range(1, len(dest) + 1):
        dp[(0, j)] = j

    for i in range(len(src)):
        for j in range(len(dest)):
            dp[(i + 1, j + 1)] = min(
                    dp[(i, j + 1)] + 1,
                    dp[(i + 1, j)] + 1,
                    dp[(i, j)] + (0 if src[i] == dest[j] else 1)
                    )

    return dp[(len(src), len(dest))]


if __name__ == '__main__':

    if len(sys.argv) < 3:
        print >>sys.stderr, 'a source file is required'
        sys.exit(1)

    print levenshtein_distance(sys.argv[1], sys.argv[2])
