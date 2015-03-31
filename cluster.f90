module cluster
  use lattice_init
  implicit none

  public grow_cluster
  public grow_cluster_new
  public increase_queue
  public decrease_queue
  public getSpin
  public addtoCluster
  public init_random_seed

contains
  subroutine grow_cluster_new(lattice,friends,N,T)
    integer, intent(in) :: N, friends(4,0:N*N-1)
    integer, intent(inout) :: lattice(N,N)
    integer :: queue(N*N)
    real(8), intent(in) :: T
    
    real(8) :: rand1, test
    integer :: rand_indx, init_spin, spin,i,j,k,site

    integer :: head, tail
    head = 1
    tail = 2
    call random_number(rand1)
    rand_indx = ceiling(rand1*(N*N))-1
    call getSpin(rand_indx,N,lattice,init_spin)
    call indx_to_coord(rand_indx,N,i,j)
    lattice(i,j) = -1*lattice(i,j)
    queue(head) = rand_indx
    do while (head<tail)
       do k = 1,4
          site = friends(k,queue(head))
          call getSpin(site,N,lattice,spin)
          if (spin == init_spin) then
             call random_number(test)
             if ((1-exp(-2d0/T)) > test) then 
                call indx_to_coord(site,N,i,j)
                lattice(i,j) = -1*lattice(i,j)
                queue(tail) = site
                tail = tail + 1
             end if
          end if
       end do
       head = head + 1
    end do
    
  end subroutine grow_cluster_new










  subroutine grow_cluster(lattice,friends,N,T)
    integer, intent(in) :: N, friends(4,0:N*N-1)
    integer, intent(inout) :: lattice(N,N)
    integer, allocatable :: queue(:)
    real(8), intent(in) :: T
    
    real(8) :: rand1, test
    integer :: rand_indx, init_spin, spin

    call random_number(rand1)
    rand_indx = ceiling(rand1*(N*N))-1
    call getSpin(rand_indx,N,lattice,init_spin)
    allocate(queue(0))

    call addtoCluster(N,rand_indx,friends,lattice,queue)
    
    do while (size(queue) > 0)
       call getSpin(queue(1),N,lattice,spin)
       if (spin == init_spin) then
          call random_number(test)
          if ((1-exp(-2d0/T)) > test) then
             call addtoCluster(N,queue(1),friends,lattice,queue) 
          end if
       end if
       call decrease_queue(queue)
    end do

  end subroutine grow_cluster

  subroutine getSpin(indx,N,lattice,spin)
    integer, intent(in) :: indx, N
    integer, intent(in) :: lattice(N,N)
    integer, intent(out) :: spin
    integer :: i,j

    call indx_to_coord(indx,N,i,j)
    spin = lattice(i,j)

  end subroutine getSpin

  subroutine addtoCluster(N,indx,friends,lattice,queue)
    integer, intent(in) :: N, indx
    integer, intent(in) :: friends(4,0:N*N-1)
    integer, intent(inout),allocatable :: queue(:)
    integer, intent(inout) :: lattice(N,N)
    integer :: i,j,k, site
    
    site=indx
    call indx_to_coord(indx,N,i,j)
    lattice(i,j) = -1*lattice(i,j)
    do k = 1, 4
       call increase_queue(queue,friends(k,site))
    end do
  end subroutine addtoCluster

  SUBROUTINE increase_queue(queue,indx)
    INTEGER,DIMENSION(:),ALLOCATABLE :: tmp_arr
    integer,allocatable,intent(inout)::queue(:)
    integer, intent(in)::indx
    integer new_dim
    integer :: i

    i = indx
    new_dim = size(queue) + 1
    ALLOCATE(tmp_arr(new_dim))

    tmp_arr(1:SIZE(queue))=queue

    DEALLOCATE(queue)
    ALLOCATE(queue(new_dim))
    queue=tmp_arr
    queue(size(queue)) = i
  ENDSUBROUTINE increase_queue

  SUBROUTINE decrease_queue(queue)
    INTEGER,DIMENSION(:),ALLOCATABLE :: tmp_arr
    integer,allocatable,intent(inout)::queue(:)
    integer :: new_dim

    new_dim = size(queue) - 1
    ALLOCATE(tmp_arr(new_dim))
    tmp_arr=queue(2:size(queue))
    DEALLOCATE(queue)
    ALLOCATE(queue(new_dim))
    queue=tmp_arr

  ENDSUBROUTINE decrease_queue

  subroutine init_random_seed()
    implicit none

    integer, allocatable :: seed(:)
    integer :: i, n, istat, dt(8), pid, t(2), s
    integer(8) :: count, tms
    call random_seed(size = n)
    allocate(seed(n))
    open(unit=30, file="/dev/urandom", access="stream",&
         form="unformatted", action="read", status="old", &
         iostat=istat)
    if (istat == 0) then
       read(30) seed
       close(30)
    else
       call system_clock(count)
       if (count /= 0) then
          t = transfer(count, t)
       else
          call date_and_time(values=dt)
          tms = (dt(1) - 1970)*365_8 * 24 * 60 * 60 * 1000 &
               + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
               + dt(3) * 24 * 60 * 60 * 60 * 1000 &
               + dt(5) * 60 * 60 * 1000 &
               + dt(6) * 60 * 1000 + dt(7) * 1000 &
               + dt(8)
          t = transfer(tms, t)
       end if
       s = ieor(t(1), t(2))
       pid = getpid() + 1099279 ! Add a prime
       s = ieor(s, pid)
       if (n >= 3) then
          seed(1) = t(1) + 36269
          seed(2) = t(2) + 72551
          seed(3) = pid
          if (n > 3) then
             seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
          end if
       else
          seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
       end if
    end if
    call random_seed(put=seed)
  end subroutine init_random_seed

end module cluster
