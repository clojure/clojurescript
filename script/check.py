#!/usr/bin/env python

import sys

data = sys.stdin.read()
for line in data.split('\n'):
    try:
        if int(line) > 0:
            sys.exit(-1)
    except ValueError:
        pass
