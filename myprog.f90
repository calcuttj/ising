program advanced_ising
  
  use lattice_init
  use cluster
  use calculations

  implicit none

  integer, parameter :: N=100
  integer :: ising_lattice(N,N), friends(4,0:(N*N-1))
  integer :: iter, iterations
  real(8) :: initial_temp = 0.01, final_temp = 4d0, T, temp_step_size = 0.01
  real(8) :: prev_mag, mag, suscept
  real(8) :: prev_energy, energy, C_v
  integer :: flag

  write(*,*) "Hit 1 for all up, -1 for all down, else for random lattice"
  read(*,*) flag

  call init_random_seed()
  T = initial_temp
  open(unit=20,file="MagVsT.txt")
  open(unit=21,file="SuscVsT.txt")
  open(unit=22,file="EnergyVsT.txt")
  open(unit=23,file="C_vVsT.txt")
  open(unit=37,file="beta.txt")
  open(unit=38,file="gamma.txt")

  do while (T <= final_temp)
     write(*,*) T
     call initialize_all(ising_lattice,N,flag,friends)
     if (T<1d0) then
        iterations = 100
        else if (T<2d0) then
           iterations = 500
        else if (T<3d0) then
           iterations = 1000
        else
           iterations = 5000 
        end if
     do iter = 1, iterations
        call grow_cluster_new(ising_lattice,friends,N,T)  
     end do

     call calc_mag(ising_lattice,N,mag) 
     
     call calc_energy(N,friends,ising_lattice,energy)
     if(T > initial_temp)then
        suscept =abs(mag-prev_mag)/temp_step_size
        C_v = (energy-prev_energy)/temp_step_size
        write(21,*) T,suscept
        write(23,*) T,abs(C_v)   
        write(38,*) log(abs(T-2.35)),log(suscept)
     end if
     prev_mag=mag
     prev_energy = energy

     write(20,*) T,abs(mag)
     write(22,*) T,abs(energy)
     write(37,*) log(abs(T-2.35)),log(mag)
     T = T + temp_step_size
  end do

end program

subroutine write_lattice(lattice,N)
  integer, intent(in)::N
  integer, intent(in)::lattice(N,N)

  integer :: i,j
  do i = 1, N
     do j =1,N
        write(*,'(I2,X)', advance='no') lattice(i,j)
     end do
     write(*,*) ''
  end do
  write(*,*) ''

end subroutine write_lattice

