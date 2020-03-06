program tension
use physics
implicit none

real,dimension(3,3,1000,1000) :: matter_field_record
real,dimension(3,3,1000) :: matter_field !1to3,1,n is position. 1to3,2,n is velocity. 1to3,3,n is acceleration. mass is 1 and size is 1x1 (unitless)
real :: start,finish,pos_neg,initial_separation=10,delta_time=0.01
integer :: particles,positions,time
character(len=1000) :: filename

call CPU_Time(start)

!setup positions
do particles=1,size(matter_field(1,1,:))
	do positions=1,size(matter_field(:,1,1))
		call random_number(pos_neg)
		call random_number(matter_field(positions,1,particles))
		matter_field(positions,1,particles)=matter_field(positions,1,particles)*initial_separation
		if (pos_neg>=0.5) then
			matter_field(positions,1,particles)=-matter_field(positions,1,particles)
		end if
	end do
end do

!create the folder and delete the old one
call EXECUTE_COMMAND_LINE("rm -r onebyone")
call EXECUTE_COMMAND_LINE("mkdir onebyone")

!time progresses by delta_time unit per entry
do time=1,size(matter_field_record(1,1,1,:))

	do particles=1,size(matter_field(1,1,:))
		call gravity(particles,matter_field)
	end do
	
	matter_field(:,2,:)=matter_field(:,2,:)+matter_field(:,3,:)*delta_time
	matter_field(:,1,:)=matter_field(:,1,:)+matter_field(:,2,:)*delta_time+0.5*matter_field(:,2,:)*delta_time**2
	matter_field_record(:,:,:,time)=matter_field
	!open the text file
	write(filename,"(A21,I0,A4)") "onebyone/field_matter",time,".txt"

	!write a single line for each particle: position, velocity, acceleration
	open(unit=10,file=filename)
	do particles=1,size(matter_field(1,1,:))
		write(10,*) matter_field(:,:,particles)
	end do
	close(10)

end do

call CPU_Time(finish)
call print_interval(start,finish)
end program
