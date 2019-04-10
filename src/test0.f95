! (test.f95) -*- coding: utf-8; mode: f90-mode -*-
! 2019-04

program test0
  use prop

  real :: g0 = 99.0, gw = 101.0

  ! 50% humidity
  ! > [g0, gw] = p676d11_ga(20.0, 1013.25, 11.508, 300.0)
  ! g0 =  0.010656
  ! gw =  0.14595

  write(*, *) '50%'
  call p676_dry(20.0, 1013.25, 11.508, 300.0, g0)
  call p676_vapor(20.0, 1013.25, 11.508, 300.0, gw)
  write(*, *) 'g₀ = ', g0
  write(*, *) 'gw = ', gw

  ! 30% humidity
  ! [g0, gw] = p676d11_ga(20.0, 1013.25, 6.9045, 300.0)
  ! g0 =  0.010588
  ! gw =  0.086713

  write(*, *) '30%'
  call p676_dry(20.0, 1013.25, 6.9045, 300.0, g0)
  call p676_vapor(20.0, 1013.25, 6.9045, 300.0, gw)
  write(*, *) 'g₀ = ', g0
  write(*, *) 'gw = ', gw

end program test0
