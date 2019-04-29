# (test-2.py) -*- coding: utf-8; mode: python-mode -*-
# test the generated Python bindings.

# (c) lloda@sarc.name 2019
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your option) any
# later version.

import sys, atmospheres, prop

def rel_error(a, b):
    if (a==b):
       e = 0
    elif (a==0.):
       e = b
    else:
       e = 2.*abs(a-b)/(abs(a)+abs(b))
    return e

ne = 0

h = 0
p, rho, temp, error = atmospheres.p835_ref(h)
ne += (7.5 != rho);
ne += (288.15 != temp);
ne += (1013.25 != p);
print("h {0:f} p {1:f} rho {2:f} temp {3:f} ne {4:d}".format(h, p, rho, temp, ne));

lat = 3.133
lon = 101.7
hr = prop.p839_rain_height(lat, lon)
rerr = rel_error(4.9579744, hr)
print("lat {0:f} lon {1:f} hr {2:f} rerr {3:s}".format(lat, lon, hr, repr(rerr)));
ne += (rerr>=1e-15)

sys.exit(ne)
