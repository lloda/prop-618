
// prop.h generated from prop.f95 by protos.scm 2019-04-28T14:30:27

#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

int32_t
prop_init
();

double
p839_rain_height
(double const * lat, double const * lon);

double
p837_rainfall_rate
(double const * lat, double const * lon);

double
p1510_temp
(double const * lat, double const * lon);

void
p838_coeffs
(double const * freq, double  * kh, double  * ah, double  * kv, double  * av);

double
p618_rain
(double const * lat, double const * lon, double const * hs, double const * freq, double const * eldeg, double const * taudeg, double const * p, double const * r001);

double
p676_vapor_pressure
(double const * rho, double const * temp);

void
p676_gas_specific
(int32_t const * scut, double const * f, double const * P, double const * e, double const * temp, double  * go, double  * gw);

void
p676_eq_height
(double const * f, double const * e, double const * P, double  * ho, double  * hw);

double
p676_gas
(double const * eldeg, double const * freq, double const * P, double const * e, double const * temp, double const * Vt, double const * hs);

double
p840_Lred
(double const * lat, double const * lon, double const * p);

double
p453_Nwet
(double const * lat, double const * lon, double const * p);

double
p840_clouds
(double const * freq, double const * eldeg, double const * Lred);

double
p618_scint
(double const * freq, double const * eldeg, double const * Deff, double const * p, double const * Nwet);

double
p1511_topoh
(double const * lat, double const * lon);

double
p836_V
(double const * lat, double const * lon, double const * p, double const * h);

#ifdef __cplusplus
} // extern "C"
#endif

// end of prop.h

