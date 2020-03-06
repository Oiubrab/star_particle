import sys
import numpy
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.animation as animation

#set up the figure
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

# Set up formatting for the movie files
Writer = animation.writers['ffmpeg']
writer = Writer(fps=15, metadata=dict(artist='Me'), bitrate=1800)

field_snapshot_scale=int(sys.argv[1])

def move_particles(frame):
	
	ax.clear()

	ax.set_xlim3d([-field_snapshot_scale, field_snapshot_scale])
	ax.set_xlabel('X')

	ax.set_ylim3d([-field_snapshot_scale, field_snapshot_scale])
	ax.set_ylabel('Y')

	ax.set_zlim3d([-field_snapshot_scale, field_snapshot_scale])
	ax.set_zlabel('Z')

	ax.set_title('3D Test')

	matter = numpy.loadtxt("onebyone/field_matter%r.txt"%(frame))
		
	ax.scatter(matter[:,0], matter[:,1], matter[:,2], zdir='z', s=5, c='blue', depthshade=True)

line_ani = animation.FuncAnimation(fig, move_particles, frames=range(1,1000), interval=1)

#plt.show()
line_ani.save('random_field_%r.mp4'%(field_snapshot_scale), writer=writer)
