module initialise
!------------------------------------------------------------------------------------------!
!------Module to initialise problem by allowing user to select desired N, direction of-----! 
!------magnetic moments and whether to run in series or parallel.--------------------------!
!------------------------------------------------------------------------------------------!
  use physical_constants

  implicit none
  
  contains
  
  !initialisation subroutine
  recursive subroutine init(mag_vector, coords, N, sORp)
     
    real(kind=dp), dimension(3), intent(out) :: mag_vector
     
    real(kind=dp), dimension(:), allocatable, intent(out) :: coords
    
    integer, intent(out) :: N
    
    character :: mag_direction
    
    character, intent(out) :: sORp
    
    !If this read statement is uncommented, will be used to read in values for N, 
    !mag_direction and sORp from bash script rather than them being entered at run time.
    !For this must also comment out all other read statements in this module
    !read (*,*) N, mag_direction, sORp
    
    !Open a file which can be 3D plotted (e.g. with gnuplot) to 
    !display the ellipsoid of points (or cube should the dimensions be smaller than the
    !ellipsoid radii.
    !open(unit=unit1, file="ellipsoid.txt", iostat=istat)
    !if(istat/=0) stop "Error opening ellipsoid.txt"
    
    !Section allows user to choose value for N
    print *, "-----------------------------------------------------------------"
    
    print *, "Enter a value for N. The total number of dipoles will be", &
             " N cubed, such that they form a cube of side N dipoles."
             
    read(*,*) N
    
    !check to make sure N is > 0
    if(N<=0)then
    
      print *, "ERROR: N may not be less than or equal to zero."
      
      print *, coords
      
      call init(mag_vector, coords, N, sORp)
      
    else
    
      print *, "N is set to:", N
      
    endif
    
    !Section allows user to choose direction of magnetic dipole moment
    print *, "-----------------------------------------------------------------"
    
    print *, "Please choose the direction of the magnetic moment of each", &
             " dipole"
             
    print *, "Type 'x', 'y' or 'z' to choose the direction. Or type 'r' to", &
             " restart."
    
    read(*,*) mag_direction
    
    if(mag_direction == "x")then
      
      mag_vector = [1.0_dp, 0.0_dp, 0.0_dp]
      
    elseif(mag_direction == "y")then
    
      mag_vector = [0.0_dp, 1.0_dp, 0.0_dp]
      
    elseif(mag_direction == "z")then
    
      mag_vector = [0.0_dp, 0.0_dp, 1.0_dp]
      
    elseif(mag_direction == "r")then
    
      call init(mag_vector, coords, N, sOrp)
    
    else
    
      print *, mag_direction, " is not a valid input."
      
      call init(mag_vector, coords, N, sORp)
      
    endif
    
    print *, "The unit vector for the magnetic moment of each dipole is"
    
    print *, mag_vector
    
    !check to see if coords has already been allocated
    !This happens if N was given a not allowed value (<=0)
    if(allocated(coords))then
     
      deallocate(coords)
      
    endif
    
    !allocate size of coordinate arrays
    allocate(coords(1:N))
    
    !set possible x, y and z coordinates
    do i = 1, N
    
      coords(i) = real(i,dp) * l
    
    enddo
    
    !initialise ellipsoid
    call ellipsoid(N, l, ellipCent)
    
    print *, "-----------------------------------------------------------------"
    
    print *, "Ellipsoid is centred on the coordinates x = y = z with x,y,z ="
    
    print *, ellipCent
    
    print *, "-----------------------------------------------------------------"
    
    print *, "Select whether to calculate in serial('s') or parallel('p').", &
             " Or type 'r' to restart."
    
    read(*,*) sORp
    
    if(sORp == "s")then
      
      print *, "Running in serial."
      
    elseif(sORp == "p")then
      
      print *, "Running in parallel."
      
    elseif(sORp == "r")then
    
      call init(mag_vector, coords, N, sORp)
      
    else
    
      print *, sORp, "is not a valid input."
      
      call init(mag_vector, coords, N, sORp)
      
    endif
    
  endsubroutine init
  
  !Subroutine to create the ellipsoid such that it is centered on the centre of the cube
  !of dipoles produced.
  subroutine ellipsoid(N, l, center)
  
    real(kind=dp), intent(in) :: l
    
    real(kind=dp) :: halfway
    
    integer, intent(in) :: N
    
    !centre point of ellipsoid, centred on the centre of the cube of dipoles
    real(kind=dp), intent(out) :: center
    
    !calculate halfway point between lowest and highest coordinate points
    halfway = (N*l) - l
    
    !set central point of the ellipsoid
    center = (halfway / 2.0_dp) + l
    
  endsubroutine ellipsoid

endmodule initialise
