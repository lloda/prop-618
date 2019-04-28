
# atmospheres.py generated from atmospheres.f95 by protos.scm

import ctypes
from ctypes import c_double, c_int32, POINTER
from ctypes.util import find_library
liba = ctypes.cdll.LoadLibrary(find_library('atmospheres'))
liba.atmospheres_init()

def atmospheres_init():
    liba.atmospheres_init()
    return 

def p835_ref(h):
    p_h = POINTER(c_double)(c_double(h))
    p_P = POINTER(c_double)(c_double(0))
    p_rho = POINTER(c_double)(c_double(0))
    p_T = POINTER(c_double)(c_double(0))
    p_error = POINTER(c_int32)(c_int32(0))
    liba.p835_ref(p_h, p_P, p_rho, p_T, p_error)
    return p_P.contents.value, \
           p_rho.contents.value, \
           p_T.contents.value, \
           p_error.contents.value

# end of atmospheres.py

