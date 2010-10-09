#!/usr/bin/env python
# coding: utf-8

import sys 
import traceback

class BrainFuck:
    def __init__(self, src):
        self.src = src
        self.src_len = len(src)
        self.src_i = 0
        self.memory_p = 0
        self.memory = [0] * 30000

        self.processes = {
                '>':self.pointer_inc,  '<':self.pointer_dec, 
                '+':self.memory_inc,   '-':self.memory_dec,
                '.':self.putchar,      ',':self.getchar,
                '[':self.jump_forward, ']':self.jump_backward,
                }
        
    def eval(self):
        while self.src_len > self.src_i:
            s = self.src[self.src_i]
            if self.processes.has_key(s):
                self.processes[s]()
            else:
                print >>sys.stderr, 'unknown instruction "%s"' % s
                sys.exit(1);
            self.src_i += 1

    def putchar(self):
        sys.stdout.write(chr(self.memory[self.memory_p]))

    def getchar(self):
        self.memory[self.memory_p] = ord(sys.stdin.read(1))

    def jump_forward(self):
        if self.memory[self.memory_p] == 0:
            self.src_i += 1
            counter = 0
            while self.src_len > self.src_i:
                s = self.src[self.src_i]
                if s == '[':
                    counter += 1
                elif s == ']':
                    if counter == 0:
                        return
                    else:
                        counter -= 1
                self.src_i += 1
            print >>sys.stderr, traceback.print_stack()
            sys.exit(1);

    def jump_backward(self):
        if self.memory[self.memory_p] != 0:
            self.src_i -= 1
            counter = 0
            while self.src_i >= 0:
                s = self.src[self.src_i]
                if s == ']':
                    counter += 1
                elif s == '[':
                    if counter == 0:
                        return
                    else:
                        counter -= 1
                self.src_i -= 1
            print >>sys.stderr, traceback.print_stack()
            sys.exit(1);

    def pointer_inc(self):
        self.memory_p += 1

    def pointer_dec(self):
        self.memory_p -= 1

    def memory_inc(self):
        self.memory[self.memory_p] += 1

    def memory_dec(self):
        self.memory[self.memory_p] -= 1

if __name__ == '__main__':

    if len(sys.argv) < 2:
        print >>sys.stderr, 'a source file is required'
        sys.exit(1)

    with open(sys.argv[1], 'r') as f:
        src = f.read()

    bf = BrainFuck(src.rstrip())
    bf.eval()
    sys.exit(0)
