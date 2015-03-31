module lattice_init

  Implicit None

  public initialize_all
  public coord_to_indx
  public indx_to_coord

  public init_lattice
  public randy
  public find_friends

contains
  subroutine initialize_all(lattice,N,spin,friends)
    integer, intent(in) :: N, spin
    integer, intent(out) :: lattice(N,N), friends(4,0:(N*N-1))

    call init_lattice(lattice,N,spin)
    call find_friends(N,friends)

  end subroutine initialize_all

  subroutine init_lattice(lattice,N,spin)
    integer, intent(in) :: N,spin
    integer, dimension(N,N), intent(out) :: lattice

    if (spin == 1 .or. spin == -1) then
       lattice = spin 
    else
       call randy(lattice,N)
    end if

  end subroutine init_lattice

  subroutine randy(lattice,N)
    integer, intent(in) :: N
    integer, dimension(N,N), intent(out) :: lattice
    integer :: i,j
    real(8) :: rand

    do i =1, N
       do j=1,N
          call random_number(rand)
          if (rand > 0.5) then
             lattice(i,j) = -1
          else 
             lattice(i,j) = 1
          end if
       end do
    end do

  end subroutine randy
  
  subroutine find_friends(N,friends)
    integer, intent(in) :: N
    integer, intent(out) :: friends(4,0:(N*N-1))
    integer :: i, j, counter, north_indx, east_indx, south_indx, west_indx

    do counter = 0, N*N-1
       call indx_to_coord(counter,N,i,j)
       if (i == 1) then !!North
          call coord_to_indx(N,j,N,north_indx)
       else
          call coord_to_indx(i-1,j,N,north_indx)
       end if
       friends(1,counter) = north_indx

       if (j == N) then !!East
          call coord_to_indx(i,1,N,east_indx)
       else
          call coord_to_indx(i,j+1,N,east_indx)
       end if
       friends(2,counter) = east_indx

       if (i == N) then !!South
          call coord_to_indx(1,j,N,south_indx)
       else 
          call coord_to_indx(i+1,j,N,south_indx)
       end if
       friends(3,counter) = south_indx

       if (j == 1) then !!West
          call coord_to_indx(i,N,N,west_indx)
       else
          call coord_to_indx(i,j-1,N,west_indx)
       end if
       friends(4,counter) = west_indx
    end do

  end subroutine find_friends

  subroutine indx_to_coord(indx,N,x,y)
    integer, intent(in) :: indx, N
    integer, intent(out) :: x,y

    x = indx/N+1
    y = mod(indx,N)+1

  end subroutine indx_to_coord

  subroutine coord_to_indx(x,y,N,indx)
    integer, intent(in) :: x,y,N
    integer, intent(out) :: indx

    indx = (x-1)*N+(y-1)

  end subroutine coord_to_indx

end module lattice_init
