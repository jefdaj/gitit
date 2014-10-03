#!/usr/bin/env python2

# Example of how to write an external PageTransform plugin in Python.

import sys

print 'Hello from python!'
print '<br/>'
print 'Your script recieved these args:'
print '<br/>'
print sys.argv
print '<br/>'
print 'And this text from stdin:'
print '<br/>'
print sys.stdin.read()
