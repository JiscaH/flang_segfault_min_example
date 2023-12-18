subroutine dostuff(N,V)
  implicit none

  integer, intent(IN) :: N
  integer, intent(INOUT) :: V(N*N)
  integer :: M(N,N)

  M = 42
  V = RESHAPE(M, (/N*N/))

end subroutine dostuff
