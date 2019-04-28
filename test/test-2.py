# (test-2.py) -*- coding: utf-8; mode: python-mode -*-
# test the generated Python bindings.

# (c) lloda@sarc.name 2019
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your option) any
# later version.

import sys, atmospheres

ne = 0
h = 0
p, rho, temp, error = atmospheres.p835_ref(h)
ne += (7.5 != rho);
ne += (288.15 != temp);
ne += (1013.25 != p);
print("h {0:f} p {1:f} rho {2:f} temp {3:f} ne {4:d}".format(h, p, rho, temp, ne));

sys.exit(ne)
