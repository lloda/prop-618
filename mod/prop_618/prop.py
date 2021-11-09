
# This file, prop.py, has been generated from prop.f90 by protos.scm

import ctypes
from ctypes import c_int32, c_double, byref
from ctypes.util import find_library
liba = ctypes.cdll.LoadLibrary(find_library('prop'))
ec = liba.prop_init()
if ec!=0:
    raise RuntimeError('prop-618 could not be initialized (error: {})'.format(ec))

def init():
    liba.prop_init.restype = c_int32
    result_ = liba.prop_init()
    return result_

def p839_rain_height(lat, lon):
    p_lat = c_double(lat)
    p_lon = c_double(lon)
    liba.p839_rain_height.restype = c_double
    result_ = liba.p839_rain_height(byref(p_lat), byref(p_lon))
    return result_

def p837_rainfall_rate(lat, lon):
    p_lat = c_double(lat)
    p_lon = c_double(lon)
    liba.p837_rainfall_rate.restype = c_double
    result_ = liba.p837_rainfall_rate(byref(p_lat), byref(p_lon))
    return result_

def p1510_temp(lat, lon):
    p_lat = c_double(lat)
    p_lon = c_double(lon)
    liba.p1510_temp.restype = c_double
    result_ = liba.p1510_temp(byref(p_lat), byref(p_lon))
    return result_

def p838_coeffs(freq):
    p_freq = c_double(freq)
    p_kh = c_double(0)
    p_ah = c_double(0)
    p_kv = c_double(0)
    p_av = c_double(0)
    liba.p838_coeffs(byref(p_freq), byref(p_kh), byref(p_ah), byref(p_kv), byref(p_av))
    return p_kh.value, \
           p_ah.value, \
           p_kv.value, \
           p_av.value

def p618_rain(lat, lon, hs, freq, eldeg, taudeg, ppc, r001):
    p_lat = c_double(lat)
    p_lon = c_double(lon)
    p_hs = c_double(hs)
    p_freq = c_double(freq)
    p_eldeg = c_double(eldeg)
    p_taudeg = c_double(taudeg)
    p_ppc = c_double(ppc)
    p_r001 = c_double(r001)
    liba.p618_rain.restype = c_double
    result_ = liba.p618_rain(byref(p_lat), byref(p_lon), byref(p_hs), byref(p_freq), byref(p_eldeg), byref(p_taudeg), byref(p_ppc), byref(p_r001))
    return result_

def p676_vapor_pressure(rho, temp):
    p_rho = c_double(rho)
    p_temp = c_double(temp)
    liba.p676_vapor_pressure.restype = c_double
    result_ = liba.p676_vapor_pressure(byref(p_rho), byref(p_temp))
    return result_

def p676_gas_specific(scut, f, P, e, temp):
    p_scut = c_int32(scut)
    p_f = c_double(f)
    p_P = c_double(P)
    p_e = c_double(e)
    p_temp = c_double(temp)
    p_go = c_double(0)
    p_gw = c_double(0)
    liba.p676_gas_specific(byref(p_scut), byref(p_f), byref(p_P), byref(p_e), byref(p_temp), byref(p_go), byref(p_gw))
    return p_go.value, \
           p_gw.value

def p676_eq_height(f, e, P):
    p_f = c_double(f)
    p_e = c_double(e)
    p_P = c_double(P)
    p_ho = c_double(0)
    p_hw = c_double(0)
    liba.p676_eq_height(byref(p_f), byref(p_e), byref(p_P), byref(p_ho), byref(p_hw))
    return p_ho.value, \
           p_hw.value

def p676_gas(eldeg, freq, P, e, temp, Vt, hs):
    p_eldeg = c_double(eldeg)
    p_freq = c_double(freq)
    p_P = c_double(P)
    p_e = c_double(e)
    p_temp = c_double(temp)
    p_Vt = c_double(Vt)
    p_hs = c_double(hs)
    liba.p676_gas.restype = c_double
    result_ = liba.p676_gas(byref(p_eldeg), byref(p_freq), byref(p_P), byref(p_e), byref(p_temp), byref(p_Vt), byref(p_hs))
    return result_

def p840_Lred(lat, lon, ppc):
    p_lat = c_double(lat)
    p_lon = c_double(lon)
    p_ppc = c_double(ppc)
    liba.p840_Lred.restype = c_double
    result_ = liba.p840_Lred(byref(p_lat), byref(p_lon), byref(p_ppc))
    return result_

def p453_Nwet(lat, lon, ppc):
    p_lat = c_double(lat)
    p_lon = c_double(lon)
    p_ppc = c_double(ppc)
    liba.p453_Nwet.restype = c_double
    result_ = liba.p453_Nwet(byref(p_lat), byref(p_lon), byref(p_ppc))
    return result_

def p840_clouds(freq, eldeg, Lred):
    p_freq = c_double(freq)
    p_eldeg = c_double(eldeg)
    p_Lred = c_double(Lred)
    liba.p840_clouds.restype = c_double
    result_ = liba.p840_clouds(byref(p_freq), byref(p_eldeg), byref(p_Lred))
    return result_

def p618_scint(freq, eldeg, Deff, ppc, Nwet):
    p_freq = c_double(freq)
    p_eldeg = c_double(eldeg)
    p_Deff = c_double(Deff)
    p_ppc = c_double(ppc)
    p_Nwet = c_double(Nwet)
    liba.p618_scint.restype = c_double
    result_ = liba.p618_scint(byref(p_freq), byref(p_eldeg), byref(p_Deff), byref(p_ppc), byref(p_Nwet))
    return result_

def p1511_topoh(lat, lon):
    p_lat = c_double(lat)
    p_lon = c_double(lon)
    liba.p1511_topoh.restype = c_double
    result_ = liba.p1511_topoh(byref(p_lat), byref(p_lon))
    return result_

def p836_rho(lat, lon, ppc, h):
    p_lat = c_double(lat)
    p_lon = c_double(lon)
    p_ppc = c_double(ppc)
    p_h = c_double(h)
    liba.p836_rho.restype = c_double
    result_ = liba.p836_rho(byref(p_lat), byref(p_lon), byref(p_ppc), byref(p_h))
    return result_

def p836_V(lat, lon, ppc, h):
    p_lat = c_double(lat)
    p_lon = c_double(lon)
    p_ppc = c_double(ppc)
    p_h = c_double(h)
    liba.p836_V.restype = c_double
    result_ = liba.p836_V(byref(p_lat), byref(p_lon), byref(p_ppc), byref(p_h))
    return result_

# end of prop.py

