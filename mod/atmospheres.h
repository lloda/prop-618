
// atmospheres.h generated from atmospheres.f95 by protos.scm

#pragma once
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

int32_t
atmospheres_init
();

void
p835_ref
(double const * h, double  * P, double  * rho, double  * T, int32_t  * error);

#ifdef __cplusplus
} // extern "C"
#endif

// end of atmospheres.h

