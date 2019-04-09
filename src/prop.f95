  ! (p676.f95) -*- coding: utf-8; mode: f90-mode -*-
  ! gfortran -std=f2008 -o p676.o p676.f95

module prop
  implicit none
contains

  subroutine p676d11(f, p, rho, T, g_o, g_w)
    implicit none
    real, intent(in) :: f     ! frequency (GHz)
    real, intent(in) :: p     ! Dry air pressure (hPa)
    real, intent(in) :: rho   ! Water vapor density (g/m³)
    real, intent(in) :: T     ! Temperature (K)
    real, intent(out) :: g_o
    real, intent(out) :: g_w


    g_o = 3.0
    g_w = 4.0

  end subroutine p676d11

end module prop
