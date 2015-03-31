module calculations
  use cluster
  implicit none

  public calc_mag
  public calc_suscept
  public calc_energy
  public calc_heat_capacity

contains

  subroutine calc_mag(lattice, N, mag)
    integer,intent(in) :: N
    integer, intent(in) :: lattice(N,N)
    real(8), intent(out) :: mag

    mag = 0
    mag = sum(lattice(:,:))
    mag = mag/(N*N)

  end subroutine calc_mag

  subroutine calc_suscept(T,mag,prev_mag,suscept)
    real(8), intent(in) :: T
    real(8), intent(in) :: mag
    real(8), intent(inout) :: prev_mag
    real(8), intent(out) :: suscept

    suscept = (mag-prev_mag)/T
    prev_mag = mag

  end subroutine calc_suscept

  subroutine calc_energy(N,friends,lattice,energy)
    integer, intent(in) :: N
    integer, intent(in) :: friends(4,0:N*N-1)
    integer, intent(in) :: lattice(N,N)
    real(8), intent(inout) :: energy
    integer :: indx1, indx2, i, spin1, spin2

    do indx1 = 0, N*N-1
       do i=1, 2
          indx2 = friends(i,indx1)
          call getSpin(indx1,N,lattice,spin1)
          call getSpin(indx2,N,lattice,spin2)
          energy = energy - spin1*spin2
       end do
    end do
    energy = energy/(N*N-1)
  end subroutine calc_energy

  subroutine calc_heat_capacity(T,energy,prev_energy,C_v)
    real(8), intent(in) :: T
    real(8), intent(in) :: energy
    real(8), intent(inout) :: prev_energy
    real(8), intent(out) :: C_v

    C_v = (energy-prev_energy)/T
    prev_energy = energy

  end subroutine calc_heat_capacity

end module calculations
