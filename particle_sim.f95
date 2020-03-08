program tension
use physics
implicit none

integer :: particle_number,time_steps
real,allocatable :: matter_field_record(:,:,:,:)
real,allocatable :: matter_field(:,:,:) !1to3,1,n is position. 1to3,2,n is velocity. 1to3,3,n is acceleration. mass is 1 and size is 1x1 (unitless)
real :: start,finish,pos_neg,initial_separation,delta_time
integer :: particles,positions,time
character(len=1000) :: filename,initial_conditions,particle_number_cha,time_steps_cha,delta_time_cha,initial_separation_cha

call CPU_Time(start)

IF(COMMAND_ARGUMENT_COUNT().NE.5)THEN
	WRITE(*,*)'Execute program by format:'
	WRITE(*,*)'./program initial_separation particle_number timesteps delta_time initial_conditions'
	WRITE(*,*)'initial condition options: random, random_2_clumps'
	STOP
ENDIF

!setup simulation parameters from command line arguments
CALL GET_COMMAND_ARGUMENT(1,initial_separation_cha)
CALL GET_COMMAND_ARGUMENT(2,particle_number_cha)
CALL GET_COMMAND_ARGUMENT(3,time_steps_cha)
CALL GET_COMMAND_ARGUMENT(4,delta_time_cha)
CALL GET_COMMAND_ARGUMENT(5,initial_conditions)
READ(initial_separation_cha,*)initial_separation
READ(particle_number_cha,*)particle_number
READ(time_steps_cha,*)time_steps
READ(delta_time_cha,*)delta_time

allocate(matter_field_record(3,3,particle_number,time_steps))
allocate(matter_field(3,3,particle_number))

!setup positions

!place the particles anywhere within the field of size initial_separation
if (initial_conditions=="random") then

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

end if

!place the particles anywhere within two regions
if (initial_conditions=="random_2_clumps") then
	
	do particles=1,size(matter_field(1,1,:))
		do positions=1,size(matter_field(:,1,1))
			call random_number(matter_field(positions,1,particles))
			matter_field(positions,1,particles)=matter_field(positions,1,particles)*initial_separation
			if (particles>size(matter_field(1,1,:))/2) then
				matter_field(positions,1,particles)=-matter_field(positions,1,particles)
			end if
		end do
	end do

end if
				

!create the folder and delete the old one
call EXECUTE_COMMAND_LINE("if [ -d 'onebyone' ]; then rm -r onebyone; fi")
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
