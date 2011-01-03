#!/usr/bin/env python
# coding: utf-8
import sqlite3
import sys
import random

con = sqlite3.connect(sys.argv[1])
cur = con.cursor()
cur.execute('select FRONT, BACK from TABLE_CARD where DIFFICULTY = 2')
words = cur.fetchall()
random.shuffle(words)
con.close()

for word in words:
    raw_input(word[0])
    print word[1]
    print

