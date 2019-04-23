! (atmospheres.f95) -*- coding: utf-8; mode: f90-mode -*-
! standard atmospheres for ITU propagation models
! 2019-04

! (c) lloda@sarc.name
! This library is free software; you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by the Free
! Software Foundation; either version 3 of the License, or (at your option) any
! later version.

module atmospheres
  use iso_c_binding
contains

  function h_geop(h) result(hp)
    real :: hp, h
    hp = (6356.766 * h)/(6356.766 + h)                            ! (1a)
  end function h_geop

  function h_geom(hp) result(h)
    real :: h, hp
    h = (6356.766 * hp)/(6356.766 - hp)                           ! (1b)
  end function h_geom

  subroutine p835_ref(h, P, rho, T, error) &
       bind(c, name='__p835_ref')

    ! Mean annual global reference atmosphere (‘standard’).
    ! Equation & table numbers from ITU-R P.835-6.

    real(C_DOUBLE), intent(in) :: h                   ! geometric height (km)
    real(C_DOUBLE), intent(out) :: P                  ! dry air pressure (hPa)
    real(C_DOUBLE), intent(out) :: rho                ! water vapor density (g/m³)
    real(C_DOUBLE), intent(out) :: T                  ! temperature (K)
    integer(C_INT32_T), intent(out):: error           ! error - 0 means none

    real, dimension(8) :: hh = (/ 0., 11., 20., 32., 47., 51., 71., 84.852 /)
    real, dimension(8) :: Th = (/ 288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65, 186.946 /)
    real, dimension(8) :: Ph = (/ 1013.25, 226.3226, 54.74980, 8.680422, 1.109106, 6.694167e-1, 3.956649e-2, 3.73405025e-3 /)

    ! check invalid input

    if (h < 0) then
       error = 1
       return
    else if (h > 100) then
       P = 0
       rho = 0
       T = 283.32
       error = 2
       return
    else
       error = 0
    end if

    block
      real :: hp
      hp = h_geop(h)

      ! two height regimes

      if (hp <= 84.852) then
         block
           integer :: i
           real :: a, b, del
           ! FIXME hh is sorted, so we could do better
           i = max(2, minloc(hh, DIM=1, MASK=(hp<=hh)))
           a = (hh(i)-hp)
           b = (hp-hh(i-1))
           del = (hh(i)-hh(i-1))
           T = (Th(i-1)*a + Th(i)*b) / del                            ! (2)
           P = exp((log(Ph(i-1))*a + log(Ph(i))*b) / del)             ! (3)
         end block
      else
         block
           real :: a0=95.571899, a1=-4.011801, a2=6.424731e-2, a3=-4.789660e-4, a4=1.340543e-6
           if (h<=91) then
              T = 186.8673                                            ! (4a)
           else
              T = 263.1905 - 76.3232 * sqrt(1 - ((h-91)/19.9429)**2)  ! (4b)
           end if
           P = exp(a0 + h*(a1 + h*(a2 + h*(a3 + h*a4))))              ! (5)
         end block
      end if
    end block

    rho = 7.5*exp(-h/2)                                               ! (6)

  end subroutine p835_ref

end module atmospheres
