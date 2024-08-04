!------------------------------------------------------------------------------!
!------Module contains functions for calculating the dot product---------------! 
!------of two vectors, and the magnitude of a vector.--------------------------!
!------------------------------------------------------------------------------!
module vectors

  use physical_constants
  
  implicit none
  
  contains
  
  !Function to calculate the dot product between two vectors, A and B.
  function dot(A,B)
  
    real(kind=dp) :: dot
    
    real(kind=dp), dimension(3) :: A, B
    
    dot = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
    
  endfunction dot
  
  !Function to calculate the magnitude of a vector A.
  function magnitude(A)
  
    real(kind=dp), dimension(3) :: A
    
    real(kind=dp) :: magnitude
    
    magnitude = sqrt(A(1)**2 + A(2)**2 + A(3)**2)
  
  endfunction magnitude
  
endmodule vectors
