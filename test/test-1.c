// (test-1.c) -*- coding: utf-8; mode: c -*-
// test the generated C headers.

// (c) lloda@sarc.name 2019
// This library is free software; you can redistribute it and/or modify it under
// the terms of the GNU Lesser General Public License as published by the Free
// Software Foundation; either version 3 of the License, or (at your option) any
// later version.

#include "atmospheres.h"
#include "prop.h"
#include <stdbool.h>
#include <stdio.h>

int main()
{
    double p, rho, temp;
    double h = 0;
    int32_t ne;

    p835_ref(&h, &p, &rho, &temp, &ne);
    ne += (7.5 != rho);
    ne += (288.15 != temp);
    ne += (1013.25 != p);
    printf("... h %f p %f rho %f temp %f ne %d\n", h, p, rho, temp, ne);

    return ne;
}
