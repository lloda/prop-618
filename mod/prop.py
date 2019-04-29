
# prop.py generated from prop.f95 by protos.scm

import ctypes
from ctypes import c_double, c_int32, POINTER
from ctypes.util import find_library
liba = ctypes.cdll.LoadLibrary(find_library('prop'))
liba.prop_init()

def prop_init():
    liba.prop_init()
    return 

def p839_rain_height(lat, lon):
    p_lat = POINTER(c_double)(c_double(lat))
    p_lon = POINTER(c_double)(c_double(lon))
    liba.p839_rain_height(p_lat, p_lon)
    return 

def p837_rainfall_rate(lat, lon):
    p_lat = POINTER(c_double)(c_double(lat))
    p_lon = POINTER(c_double)(c_double(lon))
    liba.p837_rainfall_rate(p_lat, p_lon)
    return 

def p1510_temp(lat, lon):
    p_lat = POINTER(c_double)(c_double(lat))
    p_lon = POINTER(c_double)(c_double(lon))
    liba.p1510_temp(p_lat, p_lon)
    return 

def p838_coeffs(freq):
    p_freq = POINTER(c_double)(c_double(freq))
    p_kh = POINTER(c_double)(c_double(0))
    p_ah = POINTER(c_double)(c_double(0))
    p_kv = POINTER(c_double)(c_double(0))
    p_av = POINTER(c_double)(c_double(0))
    liba.p838_coeffs(p_freq, p_kh, p_ah, p_kv, p_av)
    return p_kh.contents.value, \
           p_ah.contents.value, \
           p_kv.contents.value, \
           p_av.contents.value

def p618_rain(lat, lon, hs, freq, eldeg, taudeg, ppc, r001):
    p_lat = POINTER(c_double)(c_double(lat))
    p_lon = POINTER(c_double)(c_double(lon))
    p_hs = POINTER(c_double)(c_double(hs))
    p_freq = POINTER(c_double)(c_double(freq))
    p_eldeg = POINTER(c_double)(c_double(eldeg))
    p_taudeg = POINTER(c_double)(c_double(taudeg))
    p_ppc = POINTER(c_double)(c_double(ppc))
    p_r001 = POINTER(c_double)(c_double(r001))
    liba.p618_rain(p_lat, p_lon, p_hs, p_freq, p_eldeg, p_taudeg, p_ppc, p_r001)
    return 

def p676_vapor_pressure(rho, temp):
    p_rho = POINTER(c_double)(c_double(rho))
    p_temp = POINTER(c_double)(c_double(temp))
    liba.p676_vapor_pressure(p_rho, p_temp)
    return 

def p676_gas_specific(scut, f, P, e, temp):
    p_scut = POINTER(c_int32)(c_int32(scut))
    p_f = POINTER(c_double)(c_double(f))
    p_P = POINTER(c_double)(c_double(P))
    p_e = POINTER(c_double)(c_double(e))
    p_temp = POINTER(c_double)(c_double(temp))
    p_go = POINTER(c_double)(c_double(0))
    p_gw = POINTER(c_double)(c_double(0))
    liba.p676_gas_specific(p_scut, p_f, p_P, p_e, p_temp, p_go, p_gw)
    return p_go.contents.value, \
           p_gw.contents.value

def p676_eq_height(f, e, P):
    p_f = POINTER(c_double)(c_double(f))
    p_e = POINTER(c_double)(c_double(e))
    p_P = POINTER(c_double)(c_double(P))
    p_ho = POINTER(c_double)(c_double(0))
    p_hw = POINTER(c_double)(c_double(0))
    liba.p676_eq_height(p_f, p_e, p_P, p_ho, p_hw)
    return p_ho.contents.value, \
           p_hw.contents.value

def p676_gas(eldeg, freq, P, e, temp, Vt, hs):
    p_eldeg = POINTER(c_double)(c_double(eldeg))
    p_freq = POINTER(c_double)(c_double(freq))
    p_P = POINTER(c_double)(c_double(P))
    p_e = POINTER(c_double)(c_double(e))
    p_temp = POINTER(c_double)(c_double(temp))
    p_Vt = POINTER(c_double)(c_double(Vt))
    p_hs = POINTER(c_double)(c_double(hs))
    liba.p676_gas(p_eldeg, p_freq, p_P, p_e, p_temp, p_Vt, p_hs)
    return 

def p840_Lred(lat, lon, ppc):
    p_lat = POINTER(c_double)(c_double(lat))
    p_lon = POINTER(c_double)(c_double(lon))
    p_ppc = POINTER(c_double)(c_double(ppc))
    liba.p840_Lred(p_lat, p_lon, p_ppc)
    return 

def p453_Nwet(lat, lon, ppc):
    p_lat = POINTER(c_double)(c_double(lat))
    p_lon = POINTER(c_double)(c_double(lon))
    p_ppc = POINTER(c_double)(c_double(ppc))
    liba.p453_Nwet(p_lat, p_lon, p_ppc)
    return 

def p840_clouds(freq, eldeg, Lred):
    p_freq = POINTER(c_double)(c_double(freq))
    p_eldeg = POINTER(c_double)(c_double(eldeg))
    p_Lred = POINTER(c_double)(c_double(Lred))
    liba.p840_clouds(p_freq, p_eldeg, p_Lred)
    return 

def p618_scint(freq, eldeg, Deff, ppc, Nwet):
    p_freq = POINTER(c_double)(c_double(freq))
    p_eldeg = POINTER(c_double)(c_double(eldeg))
    p_Deff = POINTER(c_double)(c_double(Deff))
    p_ppc = POINTER(c_double)(c_double(ppc))
    p_Nwet = POINTER(c_double)(c_double(Nwet))
    liba.p618_scint(p_freq, p_eldeg, p_Deff, p_ppc, p_Nwet)
    return 

def p1511_topoh(lat, lon):
    p_lat = POINTER(c_double)(c_double(lat))
    p_lon = POINTER(c_double)(c_double(lon))
    liba.p1511_topoh(p_lat, p_lon)
    return 

def p836_V(lat, lon, ppc, h):
    p_lat = POINTER(c_double)(c_double(lat))
    p_lon = POINTER(c_double)(c_double(lon))
    p_ppc = POINTER(c_double)(c_double(ppc))
    p_h = POINTER(c_double)(c_double(h))
    liba.p836_V(p_lat, p_lon, p_ppc, p_h)
    return 

# end of prop.py

