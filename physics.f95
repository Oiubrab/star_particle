module physics
implicit none
contains

!this turns the position vectors into an absolute distance by poop
function distance(particle_here,particle_there) result(dist)
implicit none

real,dimension(*) :: particle_here(:),particle_there(:)
real :: dist,distance_squared,part_here,part_there
integer :: pos

distance_squared=0.

do pos=1,size(particle_here(:))
	part_here=particle_here(pos)
	part_there=particle_there(pos)
	distance_squared=distance_squared+(part_there-part_here)**2
	if (pos==size(particle_here(:))) then
		dist=sqrt(distance_squared)
	end if
end do

end function distance



subroutine gravity(particles_here,particles_everywhere)
real,dimension(*) :: particles_everywhere(:,:,:)
real,dimension(3) ::  distance_vector
real :: separation,gravitation !this defines the absolute separation and the gravitation
integer :: pos,particles,particles_here

particles_everywhere(:,3,particles_here)=[0.,0.,0.]

!calculate effect for every single particle on this particle
do particles=1,size(particles_everywhere(1,1,:))

	!ensure particle does not gravitate itself
	if (particles/=particles_here) then

		!this defines the vector from here to there, and the co-ordinate system
		distance_vector=particles_everywhere(:,1,particles)-particles_everywhere(:,1,particles_here)

		separation=distance(particles_everywhere(:,1,particles_here),particles_everywhere(:,1,particles))
		gravitation=1./(separation**2)

		!this is where the acceleration vector from the gravitation of each particle is added to particles_here
		particles_everywhere(:,3,particles_here)=particles_everywhere(:,3,particles_here)+gravitation*(distance_vector/separation)

	end if

end do

end subroutine gravity

!feed in a start and finish time for a time interval printout in hrs, mins, sec
subroutine print_interval(start,finish)
	real,intent(in) :: start, finish
	real :: t_sec, total_time
	integer :: t_hr, t_min
	
	total_time=finish-start
	t_hr = floor(total_time/3600)
	t_min = (total_time-t_hr*3600)/60
	t_sec = total_time-t_hr*3600-t_min*60
	print'(A24,I2,A5,I2,A7,F5.2,A4)',"simulator time elapsed =",t_hr,' hrs, ',t_min,' mins, ',t_sec,' sec'

end subroutine print_interval

end module physics
