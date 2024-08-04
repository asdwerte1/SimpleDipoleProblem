program serial
  
  use mag_field_calculations
  use vectors
  use initialise
  use physical_constants
  use omp_lib
  use omp_lib_kinds
  
  implicit none
  
  call init(mag_vector, coords, N, sORp)
  
  call main(coords, mag_vector)
  
  call finish(coords)
  
  contains
  
  !Main program
  subroutine main(coords, mag_vector)
  
    real(kind=dp), dimension(:), intent(inout) :: coords
    
    real(kind=dp), dimension(3), intent(inout) :: mag_vector
    
    !sum of magnetic field from all dipoles for calculating average
    real(kind=dp), dimension(3) :: magnet_total
    
    !average magnetic field
    real(kind=dp), dimension(3) :: magnet_ave
    
    !Demagnetisation factor
    real(kind=dp), dimension(3) :: D
    
    !timing
    real(kind=dp) :: start_time, end_time
    
    !Time at start of run
    start_time = omp_get_wtime()
    
    print *, "-----------------------------------------------------------------"
    
    print *, "Demagnetisation calculations running"
    
    !initialise magnet_sum, magnet average and demagnetisation factor
    magnet_total = [0.0_dp, 0.0_dp, 0.0_dp]
    magnet_ave = [0.0_dp, 0.0_dp, 0.0_dp]
    
    !Calculate in serial
    if(sORp == "s")then
    
      magnet_total = serial_magnet_sum(coords, mag_vector) &
                     + (self_field(mag_vector) * real(N**3, dp))
      
    !Calculate in parallel
    elseif(sORp == "p")then
    
      magnet_total = parallel_magnet_sum(coords, mag_vector) &
                     + (self_field(mag_vector) * real(N**3, dp))
      
    endif
    
    !calculate the average magnetic field over all dipoles
    magnet_ave = magnet_total / N**3
    
    print *, "Average magnetic field is:", magnet_ave
    
    D = demag(magnet_ave)
    
    print *, "-----------------------------------------------------------------"
    
    print *, "Demagnetisation factor is:", D
    
    end_time = omp_get_wtime()
    
    print *, "Time taken:", end_time - start_time, "s"  
    
  endsubroutine main
  
  subroutine finish(coords)
  
    real(kind=dp), dimension(:), allocatable, intent(inout) :: coords
    
    close(unit=unit1, iostat=istat)
    if(istat/=0) stop "Error closing ellipsoid.txt"
    
    deallocate(coords)
  
  endsubroutine finish
  
  function serial_magnet_sum(coords, mag_vector)

    real(kind=dp), dimension(:) :: coords
    
    real(kind=dp), dimension(3) :: mag_vector
    
    real(kind=dp), dimension(3) :: current_dipole
    
    real(kind=dp), dimension(3) :: serial_magnet_sum
    
    real(kind=dp), dimension(3) :: current_ext_field
    
    real(kind=dp) :: z_component_sum
    
    do i = 1, N
    
      current_dipole(1) = coords(i)
      
      do j = 1, N
        
        current_dipole(2) = coords(j)
          
        do k = 1, N
            
          current_dipole(3) = coords(k)
              
          !check to see if the current dipole is within the ellipsoid,
          !otherwise the current dipole is ignored, i.e. m = 0.
          if(ellipsoidcheck(current_dipole, rx, ry, rz, ellipCent) &
                                <= 1.0_dp)then
            
            current_ext_field = magnetic_field(current_dipole, mag_vector)
            
            serial_magnet_sum = serial_magnet_sum + current_ext_field
                    
          endif
              
        enddo
            
      enddo
          
    enddo
    
  endfunction serial_magnet_sum
  
  function parallel_magnet_sum(coords, mag_vector)

    real(kind=dp), dimension(:) :: coords
    
    real(kind=dp), dimension(3) :: mag_vector
    
    real(kind=dp), dimension(3) :: current_dipole
    
    real(kind=dp), dimension(3) :: parallel_magnet_sum
    
    integer :: nthreads, threadNum
    
    print *, "Check number of threads:"
    !Check number of threads
    !$omp parallel default(private)
  
      nthreads = omp_get_num_threads()
      
      threadNum = omp_get_thread_num()
      
      print *, "Thread", threadNum+1, "of", nthreads
    
    !$omp end parallel
    
    !$omp parallel do shared(coords,mag_vector) private(i,j,k,current_dipole) reduction(+:parallel_magnet_sum)
    do i = 1, N
    
      current_dipole(1) = coords(i)
      
      do j = 1, N
        
        current_dipole(2) = coords(j)
        
        do k = 1, N
            
          current_dipole(3) = coords(k)
          
          !check to see if the current dipole is within the ellipsoid,
          !otherwise the current dipole is ignored, i.e. m = 0.
          if(ellipsoidcheck(current_dipole, rx, ry, rz, ellipCent) &
                                <= 1.0_dp)then
            parallel_magnet_sum = parallel_magnet_sum + &
                                  magnetic_field_parallel&
                                  (current_dipole,mag_vector)
          
          endif
          
           
        enddo
           
      enddo
          
    enddo
    !$omp end parallel do
  endfunction parallel_magnet_sum
  
endprogram serial
