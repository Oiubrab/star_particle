# Star Particle Simulation
A star simulation tool to explore galactic evolution

WHAT DOES THIS DO?
-------------------

This simple set of scripts creates a set of 1000 point particles and places them randomly in a <size>^3 unit cube. the particles interact through newtonian gravitational-like force alone, with an inverse square distance law. The particles can be thought of as stars in a scaled unit field. This means:

- Each particle is equivalent and has a mass of 1 'unit' (i.e unitless)
- Distance is unitless
- Gravitatational constant, G=1

The particles are placed in the field with zero initial velocity and acceleration and are progressed through time, by computing all the gravitationally determined accelereration vectors for a particle, adding all these vectors together to get the net accceleration, and updating the particle's position and velocity accordingly.

This simulation represents the gravitational collapse of stars in a bound field. Even at rest, the random placement of stars contains some amount of gravitational potential (in a gravitational field) that confers some net non zero angular momentum state in the population, given the stars are not symmetrical in position and momentum. Thus, in the short term, those stars that are not ejected from the group in hyperbolic trajectories begin to orbit around eachother in random planes around a common centre of mass. This simulation hence evolves into a state similar to an elliptical galaxy.

As yet, this code simply updates all vectors based on current position in a millisecond resolution timeframe. This means errors are naturally going to pervade this simulation and and some eulerian solver may be more accurate. It is also noted that the error here is determined by the courant factor (approx: velocity x distance_step / time_step) which is less than one for most particles in most of the simulation. It may be possible that courant factors exceeding 1 for particles contributes to the violent expulsion of some of the particles.

HOW TO RUN?
-----------

There are two ways to get cool output here. At each step, if you need to see running instructions, just run the binary (fortran) or script (bash,python) and instructions will be outputted.

### The Little Less Easy Way 

In order to run the simulation, compile the particle_sim.f95 with the physics.f95 module and run the output. If you have gfortran this would be:

	gfortran physics.f95 particle_sim.f95 -o <your_name_here>
	./<your_name_here> <initial_separation> <particle_number> <timesteps> <delta_time> <initial_conditions> 

This will create a folder with a set of text files (for each timestep) that contaion the position, velocity and acceleration information for the particles requested by particle_number, at the initial_separation requested. As of writing this, there are two initial conditions setup here. The 'random' initial_conditions input will simply place particles randomly throughout the field defined by initial_separation. The 'random_2_clumps' Initial_conditions input splits the particles into two even groups and places them in diagonally opposed squares. In order to visualise the data, run the python_plotter.py script:

	python python_plotter.py <field_snapshot_scale> <particle_number> <timesteps> <delta_time> <initial_conditions>

to first view the simulation in a window, then output an mp4 of the particle simulation when the screen is closed. Input a number, in place of field_snapshot_scale, to determine the dimensions of the viewing area. All other input variables should be the same as hat inputted into the fortran code.

### The Easy Way

If you are doing this in bash, a convenient bash script has been written to defacate the above program all over your computer in one go. Just run:

	sh execution <field_snapshot_scale> <initial_separation> <particle_number> <timesteps> <delta_time> <initial_conditions>

### What You Need To Suceed (Or, What I Used)

Bash:

- Bash

Fortran:

- Gfortran

Python:

- python 2.7.14
- numpy 1.16.4
- matplotlib 2.0.2
