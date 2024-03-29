import sys
import os
import numpy
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.animation as animation
import time
import math as mt

# record time at start
t_start = time.time()

no_arg = len(sys.argv)
initial_arg=sys.argv[7]!='random' and sys.argv[7]!='random_2_clumps' and sys.argv[7]!='galaxy_2_clumps_edge'
preview_arg=sys.argv[8]!='yes' and sys.argv[8]!='no'

if (no_arg != 9 or initial_arg  or preview_arg):
	print("Execute program by format:")
	print("python python_plotter.py field_snapshot_scale initial_separation particle_number timesteps delta_time finesse_factor initial_conditions preview")
	print("initial condition options: random, random_2_clumps")
	print("preview options: yes, no")
	print(sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4],sys.argv[5],sys.argv[6],sys.argv[7],sys.argv[8])
	sys.exit()
else:
	#record the simulation parameters
	field_snapshot_scale=float(sys.argv[1])
	initial_separation = float(sys.argv[2])
	matter_number = int(sys.argv[3])
	matter_time = int(sys.argv[4])
	delta_time = float(sys.argv[5])
	finesse_factor = int(sys.argv[6])
	initial_conditions = sys.argv[7]
	preview = sys.argv[8]

#set up the figure
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

#the function that moves the animation forward between frames
#creates a new scatter plot and plots the points from each frame
def move_particles(frame):	
	
	ax.clear()

	ax.set_xlim3d([-field_snapshot_scale, field_snapshot_scale])
	ax.set_xlabel('X')

	ax.set_ylim3d([-field_snapshot_scale, field_snapshot_scale])
	ax.set_ylabel('Y')

	ax.set_zlim3d([-field_snapshot_scale, field_snapshot_scale])
	ax.set_zlabel('Z')

	ax.set_title('Scaled Unit Gravitational Simulator')

	matter = numpy.loadtxt("onebyone/field_matter%r.txt"%(frame))
		
	ax.scatter(matter[:,0], matter[:,1], matter[:,2], zdir='z', s=5, c='blue', depthshade=True)

if preview=='yes':
	try:
		
		#setup the animation with the frame number representing the output number in the file name
		ani = animation.FuncAnimation(fig, move_particles, frames=range(1,matter_time), interval=delta_time*1000)
		plt.show()

	except:
		#movin' right along
		pass

#set up the second figure
fig2 = plt.figure()
ax = fig2.add_subplot(111, projection='3d')

# Set up formatting for the movie files
Writer = animation.writers['ffmpeg']
writer = Writer(fps=15, metadata=dict(artist='Adrian Barbuio'), bitrate=1800)

#setup the animation with the frame number representing the output number in the file name
ani2 = animation.FuncAnimation(fig2, move_particles, frames=range(1,matter_time), interval=delta_time*1000)

#save the animation with labels for size, particle number and number of timesteps
ani2.save('size_%r_particles_%r_time_%r_dt_%r_initial_%s_separation_%r_finesse_%r.mp4'%(field_snapshot_scale,matter_number,matter_time,delta_time,initial_conditions,initial_separation,finesse_factor), writer=writer)

# record time at end, calculate elapsed time and print
t_end = time.time()
t_elapsed = t_end - t_start
t_hr = mt.floor(t_elapsed//3600)
t_min = mt.floor((t_elapsed%3600)/60)
t_sec = t_elapsed%60
print("plotter time elapsed = %s hrs, %s mins, %s s" %(t_hr,t_min,t_sec))
