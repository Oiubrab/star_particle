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

if (no_arg != 6):
	print("Execute program by format:")
	print("python python_plotter.py field_snapshot_scale particle_number timesteps delta_time initial_conditions")
	print("initial condition options: random, random_2_clumps")
	sys.exit()
else:
	#record the simulation parameters
	field_snapshot_scale=float(sys.argv[1])
	matter_number = int(sys.argv[2])
	matter_time = int(sys.argv[3])
	delta_time = float(sys.argv[4])
	initial_conditions = sys.argv[5]

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

try:
	
	#setup the animation with the frame number representing the output number in the file name
	ani = animation.FuncAnimation(fig, move_particles, frames=range(1,matter_time), interval=delta_time*1000)
	plt.show()

except:

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
ani2.save('size_%r_particles_%r_time_%r_dt_%r_initial_%s.mp4'%(field_snapshot_scale,matter_number,matter_time,delta_time,initial_conditions), writer=writer)

# record time at end, calculate elapsed time and print
t_end = time.time()
t_elapsed = t_end - t_start
t_hr = mt.floor(t_elapsed//3600)
t_min = mt.floor((t_elapsed%3600)/60)
t_sec = t_elapsed%60
print("plotter time elapsed = %s hrs, %s mins, %s s" %(t_hr,t_min,t_sec))
