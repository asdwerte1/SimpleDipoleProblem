!------------------------------------------------------------------------------!
!Module contains functions for calculating magnetic fields and for calculating-!
!the demagnetisation factor.---------------------------------------------------!
!------------------------------------------------------------------------------!
module mag_field_calculations

  use physical_constants
  
  use vectors
  
  use omp_lib
  
  use omp_lib_kinds

  implicit none
  
  contains
  
  !function to calculate the demagnetisation factor (in 3D) from a read in magnetic field
  !vector B. This is the average magnetic field vector.
  function demag(B)
  
    real(kind=dp), dimension(3) :: demag
    
    real(kind=dp), dimension(3) :: B
    
    real(kind=dp) :: magnetisation
    
    !calculate magnetisation
    magnetisation = BohrMag / l**3
    
    demag = 1.0_dp - (B / (PFS * magnetisation))
    
  endfunction demag
  
  !function to calculate the magnetic field at a dipole as a result of the same
  !dipole
  function self_field(mag_vector)
  
    real(kind=dp), dimension(3) :: self_field, mag_vector
    
    real(kind=dp), dimension(3) :: bracket
    
    real(kind=dp) :: coef
    
    coef = (PFS * BohrMag) / (4.0_dp * pi * l**3)
    
    bracket = ((8.0_dp * pi) / 3.0_dp) * mag_vector
    
    self_field = coef * bracket
    
  endfunction self_field
  
  !function to calculate the magnetic field inbetween two dipoles, current
  !dipole and one that is at position r from current dipole
  function magnetic_field(pos, mag_vector)
  
    real(kind=dp), dimension(3) :: magnetic_field
  
    !pos is the coordinates of the current dipole, mag_vector is the
    !magnetic moment unit vector
    real(kind=dp), dimension(3) :: pos, mag_vector
    
    !vector to update for each other dipole
    real(kind=dp), dimension(3) :: dipoleExt
    
    !vector r, vector from current dipole to all others
    real(kind=dp), dimension(3) :: r
    
    !unit vector of r
    real(kind=dp), dimension(3) :: unit_r
    
    !magnitude of vector r
    real(kind=dp) :: r_mag
    
    real(kind=dp) :: coef
    
    real(kind=dp), dimension(3) :: numerator
    
    integer :: o, p, q
    
    !initialise magnetic field
    magnetic_field = 0.0_dp
    
    !Loop over all external dipoles.
    do o = 1, N
    
      dipoleExt(1) = coords(o)
      
      do p = 1, N
      
        dipoleExt(2) = coords(p)
      
        do q = 1, N
        
          dipoleExt(3) = coords(q)
          
          !check to see if current external dipole is within the ellipsoid
          if(ellipsoidcheck(dipoleExt, rx, ry, rz, ellipCent)<=1.0_dp)then
          
            !calculate vector r
            r = dipoleExt - pos
            
            !calculate magnitude of vector r
            r_mag = magnitude(r)
            
            !if magnitude of r is 0, i.e. r=[0,0,0], don't do calculations as 
            !maths errors will occur
            if(r_mag /= 0.0_dp)then
              
              !calculate r unit vector
              unit_r = r / r_mag
              
              !calculate magnetic field between two dipoles
              coef = (PFS * BohrMag) / (4.0_dp * pi)
              
              numerator = (3.0_dp * dot(unit_r, mag_vector) * unit_r) &
                          - mag_vector
              
              magnetic_field = coef * (numerator / r_mag**3)
              
            endif
            
          endif
        
        enddo
        
      enddo
      
    enddo
    
  endfunction magnetic_field
  
  !Function to calculate the magnetic field between 2 dipoles, at position pos and dipoleExt
  !in parallel.
  function magnetic_field_parallel(pos, mag_vector)

  real(kind=dp), dimension(3) :: magnetic_field_parallel
  
  !pos is coordinates of the current dipole, mag_vector is the magnetic moment
  !of the unit vector
  real(kind=dp), dimension(3) :: pos, mag_vector
  
  !vector to update for each other dipole
  real(kind=dp), dimension(3) :: dipoleExt
  
  !vector, r, from current dipole to all others
  real(kind=dp), dimension(3) :: r
  
  !unit vector of r
  real(kind=dp), dimension(3) :: unit_r
  
  !magnitude of vector r
  real(kind=dp) :: r_mag
  
  real(kind=dp) :: coef
  
  real(kind=dp), dimension(3) :: numerator
  
  integer :: o, p, q
  
  !initialise magnetic field
  magnetic_field_parallel = 0.0_dp
  
  !$omp parallel do
  do o = 1, N
  
    dipoleExt(1) = real(o, dp) * l
    
    !$omp parallel do
    do p = 1, N
    
      dipoleExt(2) = real(p, dp) * l
      
      !$omp parallel do
      do q = 1, N
      
        dipoleExt(3) = real(q, dp) * l
        
        !check to see if current external dipole is within the ellipsoid
        if(ellipsoidcheck(dipoleExt, rx, ry, rz, ellipCent)<=1.0_dp)then
        
          !calculate vector r
          r = dipoleExt - pos
          
          !calculate magnitude of r
          r_mag = magnitude(r)
          
          !if magnitude r = 0 skip doing calculations
          if(r_mag /= 0.0_dp)then
          
            !calculate r unit vector
            unit_r = r / r_mag
            
            !calculate the magnetic field
            coef = (PFS * BohrMag) / (4.0_dp * pi)
            
            numerator = (3.0_dp * dot(unit_r, mag_vector) * unit_r) &
                        - mag_vector
                        
            magnetic_field_parallel = coef * (numerator / r_mag**3)
            
          endif
          
        endif
        
      enddo
      !$omp end parallel do
      
    enddo
    !$omp end parallel do
    
  enddo
  !$omp end parallel do
  
  endfunction magnetic_field_parallel
  
  !Function checks if the input position vector lies within the boundary of the ellipsoid.
  function ellipsoidcheck(pos, rx, ry, rz, origin)
  
    real(kind=dp), dimension(3) :: pos
    
    real(kind=dp) :: rx, ry, rz
    
    real(kind=dp) :: origin
    
    real(kind=dp) :: ellipsoidcheck
    
    ellipsoidcheck = ((pos(1) - origin)**2 / rx**2) + &
    ((pos(2) - origin)**2 / ry**2) + ((pos(3) - origin)**2 / rz**2)
  
  endfunction ellipsoidcheck
  
endmodule mag_field_calculations
