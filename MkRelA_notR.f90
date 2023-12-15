program main
  implicit none

  integer :: N_ind, N_rel, nArg, i, x
  integer, allocatable :: ped_V(:), rel_V(:)
  character(len=32) :: arg
  character(len=2000) :: PedFileName

  PedFileName  = "NoFile"
  N_rel = 8

  nArg = command_argument_count()
  i = 0
  do x = 1, nArg
    i = i+1
    if (i > nArg)  exit
    call get_command_argument(i, arg)
    
    select case (arg)    
      
      case ('--pedigree')  
        i = i+1
        call get_command_argument(i, PedFileName)
      
      case ('--GenBack2')
        N_rel = 19
        
      case default
        print '(2a, /)', 'Unrecognised command-line option: ', arg
        stop

    end select
  end do

  N_ind = FileNumRow(trim(PedFileName)) -1  ! 1st row = header
  
  allocate(ped_V(2*N_ind))
  allocate(rel_V(N_ind*N_ind*N_rel))
  
  call ReadPedFile(PedFileName, N_ind, ped_V)  

  call getrelz(N_ind, ped_V, N_rel, rel_V)
  
  ! write results to file
  ! (not relevant)

  deallocate(ped_V)
  deallocate(rel_V)
  
    print *, 'done.'


  !~~~~~~~~~~~~~~~~~~~~
  contains
    integer function FileNumRow(FileName)
      implicit none

      character(len=*), intent(IN) :: FileName
      integer :: nrow, i, maxRow, IOerr
      character(len=42) :: dumC

      maxRow = 5000000  ! fail safe
      nrow = 0
      open(unit=102, file=trim(FileName), status="old")
      do i=1, maxRow
        read(102,*,IOSTAT=IOerr) dumC
        if (IOerr < 0) then
          exit  ! EOF
        else
          nrow = nrow +1  
        end if
      enddo
      close(102)
      FileNumRow = nrow

    end function FileNumRow

end program main

!===============================================================================

subroutine getrelz(nind, pedrf, nrel, relv)
  implicit none

  integer, intent(IN) :: nind, nrel
  integer, intent(INOUT) :: relv(nind*nind*nrel)  ! 0/1 matrix
  integer, intent(IN) :: pedrf(nInd*2)
  integer :: ped(nInd, 2), rel(nInd, nInd, nrel)
  logical :: doGP 
  integer :: i, j, x, y, r, GPr(2,2)

  ! relationships:
  ! 1 = self
  ! 2 = dam
  ! 3 = sire
  ! 4 = offspring

  ! 5 = full sib
  ! 6 = maternal half sib
  ! 7 = paternal half sib
  ! 8 = 'cross' half sib (hermaphrodite)

  ! 9  = MGM
  ! 10 = MGF
  ! 11 = PGM
  ! 12 = PGF
  ! 13 = GO

  ! 14 = FA  + dHA
  ! 15 = FN
  ! 16 = HA
  ! 17 = HN
  ! 18 = dFC1
  ! 19 = FC1

  GPr(1,1) = 9
  GPr(1,2) = 10
  GPr(2,1) = 11
  GPr(2,2) = 12

  ! fold pedigree
  ped(:,1) = pedrf(1:nInd)
  ped(:,2) = pedrf((nInd+1) : (2*nInd))

  if (nrel == 8) then
    doGP = .FALSE.
  else
    doGP = .TRUE.
  endif

  rel = 0
  do i = 1, nInd
    rel(i,i,1) = 1   ! self
    if (ped(i,1) /= 0) then
      rel(i, ped(i,1), 2) = 1  ! dam
      rel(ped(i,1), i, 4) = 1  ! offspring
    endif
    
    if (ped(i,2) /= 0) then
      rel(i, ped(i,2), 3) = 1  ! sire
      rel(ped(i,2), i, 4) = 1  ! offspring
    endif
  enddo

  ! sibs  
  do i = 1, nInd
    if (all(ped(i,:) == 0))  cycle
    
    do j = i+1, nInd
      if (all(ped(j,:) == 0))  cycle
      
      do x=1,2
        if (ped(i,x) == ped(j,x) .and. ped(i,x)/=0) then
          rel(i,j,5+x) = 1  ! mat/pat HS
        endif
      enddo
      if (rel(i,j,6) == 1 .and. rel(i,j,7) == 1) then    ! full sibs
        rel(i,j,5) = 1
        rel(i,j, 6:7) = 0
      endif
      
      ! hermaphrodites
      do x=1,2
        if (ped(i,x) == ped(j,3-x) .and. ped(i,x)/=0) then
          if (rel(i,j,8) == 1) then
            rel(i,j,5) = 1  ! cross FS  
            rel(i,j,8) = 0
          else
            rel(i,j,8) = 1  ! cross HS
          endif
        endif
      enddo
      
      do r=5,8
        rel(j,i,r) = rel(i,j,r)
      enddo

    enddo
  enddo

  ! grandparents
  if (doGP) then
    do i = 1, nInd
      do x = 1,2  
        if (ped(i,x) == 0)  cycle
        do y = 1,2
          j = ped (ped(i,x), y)  ! ID number of GP
          if (j == 0)  cycle
          rel(i,j, GPr(x,y) ) = 1  
          rel(j, i, 13) = 1  ! GO 
        enddo
      enddo
    enddo
  endif

  ! avuncular   
  if (doGP) then
    do i = 1, nInd
      do x = 1,2
        if (ped(i,x) == 0)  cycle
        
        do j = 1, nInd
          if (rel(ped(i,x), j, 5) == 1) then
            rel(i,j,14) = 1  ! FA
            rel(j,i,15) = 1  ! FN
          else if (any(rel(ped(i,x), j, 6:8) == 1)) then
            if (x==2 .and. rel(i,j,16) == 1) then
              rel(i,j,14) = 1  ! double HA
              rel(j,i,15) = 1  
            endif
            rel(i,j,16) = 1  ! HA
            rel(j,i,17) = 1  ! HN
          endif
          
          do y = 1,2
            if (ped(j,y) == 0)  cycle
            if (rel(ped(i,x), ped(j,y), 5) == 1) then
              if (rel(i,j,19) == 1)  rel(i,j,18) = 1  ! double full cousins
              rel(i,j,19) = 1  ! full cousins
            endif
          enddo
        enddo
      enddo
    enddo
  endif

  ! unfold rel
  relV = 0
  do r = 1,nrel
    do j = 1,nInd
      do i = 1,nInd  
        x = ((r-1)*nInd + (j-1))*nInd + i
        relV(x) = rel(i,j,r)
      enddo
    enddo
  enddo


end subroutine getrelz

!===============================================================================

subroutine ReadPedFile(FileName, nInd, Parent_V)
  implicit none

  character(len=*), intent(IN) :: FileName
  integer, intent(IN) :: nInd
  integer, intent(OUT) :: Parent_V(2*nInd)
  integer :: i, k, IOerr, Parent(nInd,2)
  character(len=40) :: NamePed(3,nInd)

  NamePed = "NA"
  open(unit=103, file=trim(FileName), status="old")
    read(103,*)   ! header                              
    do i=1,nInd
      read(103, *,IOSTAT=IOerr)  NamePed(:,i)
      if (IOerr > 0) then
        print *, "Wrong input in file "//trim(FileName)//" on line ", i
        stop
      end if
    enddo
  close(103)
  
  ! parent names to numbers
  Parent = 0 
  do i=1,nInd
    do k=1,2
      Parent(i,k) = NameToNum(NamePed(k+1,i)) 
    enddo
  enddo
  
  Parent_V(1:nInd) = Parent(:,1)
  Parent_V((nInd+1):(2*nInd)) = Parent(:,2)

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  contains
    function NameToNum(Navn)
      implicit none
    
      character(len=40), intent(IN) :: Navn
      integer :: NameToNum
      integer :: j

      NameToNum = 0
      if (Navn == "NA")  return
      
      do j=1, nInd
        if (Navn == NamePed(1,j)) then
          NameToNum = j
          exit
        endif
      enddo

    end function NameToNum
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
end subroutine ReadPedFile

!===============================================================================