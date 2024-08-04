module physical_constants

  implicit none
  
  !precision parameter so all reals have same level of precision
  integer, parameter :: dp=selected_real_kind(15,300)
  
  !pi
  real(kind=dp), parameter :: pi = 3.14159265358979_dp
  
  !Permiability of free space
  real(kind=dp), parameter :: PFS = 4.0e-7_dp * pi
  
  !electron charge
  real(kind=dp), parameter :: e = 1.602176634e-19
  
  !Planck constant
  real(kind=dp), parameter :: h = 6.62607015e-34
  
  !reduced Planck constant
  real(kind=dp), parameter :: h_bar = h / (2.0_dp * pi)
  
  !electron rest mass
  real(kind=dp), parameter :: m_e = 9.109383701528e-31
  
  !speed of light
  real(kind=dp), parameter :: SOL = 2.99792458e8
  
  !Bohn magneton
  real(kind=dp), parameter :: BohrMag = (e * h_bar) / (2.0_dp * m_e)
  
!------------------------------------------------------------------------------!

  !integers for files
  integer, parameter :: unit1=10, unit2=11, unit3=12
  
  integer :: istat
  
!------------------------------------------------------------------------------!

  !integers for counting
  integer :: i, j, k
    
!------------------------------------------------------------------------------!
!--All constants below this point are for use within the dipole field problem--!
!------------------------------------------------------------------------------!  
  !atomic spacing = 3 angstroms
  real(kind=dp), parameter :: l = 3.0e-10_dp
  
  !number of dipoles = N**3
  integer :: N
  
  !arrays for x, y and z possible coordinates to be stored in
  real(kind=dp), dimension(:), allocatable :: coords
  
  !magnetic moment unit vector
  real(kind=dp), dimension(3) :: mag_vector
  
  !center of the ellipsoid
  real(kind=dp) :: ellipCent
  
  !Ellipsoid radii
  real(kind=dp), parameter :: rx = 10.0e-9_dp, ry = 10.0e-9_dp, rz = 20.0e-9_dp
  
  !Defined by user in initialise module, selects if run is series or parallel
  character :: sORp
  
endmodule physical_constants
