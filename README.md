# NovelBot

The goal of this little project is to train a simple robot to navigate it's way
out of a room (or maze in the future) using novelty search.

Right now, I have a simulation of a several robots that bounce around a room and
that explode when they touch one of the vertical walls. The next steps are to:
- Give the little robots a brain
- Evolve the brains with a genetic algorithm and a fixed fitness function
- Evolve the brains using novelty search

All the code is done in Haskell at the moment using the FunGEn library for
making games. This is most likely not the best or most efficient way to code
this thing, but it's fun! 

# Braains

So how do we give the little robots a brain? There are many ways to model this
(going back to the Explaining Intelligence course) but to make things simple we
will stick to the Sense-Think-Act model where each time step the robot gets to:

1) Sense: the robot gets to look in 4 directions around it and gets the
distance to the nearest wall
2) Think: processes the information from its senses
3) Acts: modifies its speed property
