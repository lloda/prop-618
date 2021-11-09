
# This file, atmospheres.py, has been generated from atmospheres.f90 by protos.scm

import ctypes
from ctypes import c_int32, c_double, byref
from ctypes.util import find_library
liba = ctypes.cdll.LoadLibrary(find_library('atmospheres'))
ec = liba.atmospheres_init()
if ec!=0:
    raise RuntimeError('prop-618 could not be initialized (error: {})'.format(ec))

def init():
    liba.atmospheres_init.restype = c_int32
    result_ = liba.atmospheres_init()
    return result_

def p835_ref(h):
    p_h = c_double(h)
    p_P = c_double(0)
    p_rho = c_double(0)
    p_temp = c_double(0)
    p_error = c_int32(0)
    liba.p835_ref(byref(p_h), byref(p_P), byref(p_rho), byref(p_temp), byref(p_error))
    return p_P.value, \
           p_rho.value, \
           p_temp.value, \
           p_error.value

# end of atmospheres.py

