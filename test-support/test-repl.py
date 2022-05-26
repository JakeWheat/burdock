#!/usr/bin/env python3

"""x

definitely want to try to do an expect in burdock

looked at the documentation for pexpect, and it looks very difficult
to do something general

it's probably fairly easy to write a numpty expect that works well
enough for burdock's automated testing needs, which might be better

this also serves as an example of how to do pexpect using burdock,
which is really useful even if it's not needed for the burdock testing
itself

"""

import pexpect

import sys

if len(sys.argv) != 2:
    raise Exception("expected 1 arg, got: " + str(sys.argv))

def do_repl_test(lst):
    child = pexpect.spawn('_build/burdock')
    for l in lst:
        child.expect("b > ", timeout=0.1)
        child.sendline(l[0])
        child.expect(l[1], timeout=0.1)
    child.expect("b > ", timeout=0.1)
    child.sendcontrol("d")
    child.close() # add timeout

do_repl_test(eval(sys.argv[1]))
    
#do_repl_test([("1 + 2", "3")])

#do_repl_test([("a = 5", ""), ("a + 3", "8")])

#do_repl_test(eval('[("1 + 2", "3")]'))

 
#do_repl_test([("import lists", ""), ("a + 3", "8")])
