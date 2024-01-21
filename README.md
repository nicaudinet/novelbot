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

# Simulation

## Roadmap

- [x] Make the walls
- [x] Make the robot
- [x] Make the robot explode when it touches the vertical walls
- [x] Ensure that if the robot speed is large it cannot just jump over the walls

# Braains

So how do we give the little robots a brain? There are many ways to model this
(going back to the Explaining Intelligence course) but to make things simple we
will stick to the Sense-Think-Act model where each time step the robot gets to:

1) **Sense**: the robot gets to look in 4 directions around it and gets the
distance to the nearest wall
2) **Think**: processes the information from its senses
3) **Acts**: modifies its speed property

## Roadmap for the little robot brains

- [x] Make the robots gather accurate sensory data
- [x] Implement noop sense -> think -> act loop
- [x] Make a simple brain so that the robot no longer touches the walls
- [x] Make the brain into a neural network

## Sensing

In our case, the robot has 4 "eyes" pointing north, south, east and west. Each
eye essentially measures the distance between the robot and the closest wall in
that direction. The distance is either finite if there is a wall in that
direction or infinite if not.

```
                                  N
                                  ^
                                  |
                            W <-- R --> E
                                  |
                                  v
                                  S
```

## Thinking

To begin with, the brain of the little robot will be a simple linear function of
the distance to closest walls and its speed. The output is new speed for the
robot. Therefore, the brain will be represented by a total of 12 numbers (6
weights for each speed output).

Inputs: the 4 distances to the nearest wall in the cardinal directions (N,S,W,E)
and the 2 speed coordinates (X,Y). 6 total inputs.

Outputs: the 2 new speed coordinates (X',Y').

## Acting

Acting is simple: use the output speed of the thinking process as the new speed.
NOTE: this assumes the robot can have infinite acceleration.

# Evolution

After giving the robots a little brain, the main task becomes to evolve the
robots with a genetic algorithm. A nice, simple task for that would be to put
the robots in a closed room, give them a fixed minimum speed and then train them
to never crash into the walls.

- Encoding: a vector containing the brain weights
- Phenotype: the brain matrix (using the nice Squash / UnSquash typeclasses)
- Init: random numbers above the speed threshold
- Fitness function: the amount of time the robot stays alive
    - Lets start with a linear function over this and see how it goes
- Selection: tournament selection
- Cross-over: randomly choose a crossover point and swap the second half
- Mutation: small random change per weight
- Replacement: generational replacement (change all the population)
- Elitism: include copy of best individuals from previous generation

## Roadmap for the genetic algorithm

- [ ] Make it so that the robots are always moving (set a minimum speed above
    zero)
- [x] Implement the encoding / decoding from the phenotype (Brain) to genotype
  (vector of weights)
- [ ] Initialise brains
- [ ] Implement the fitness function (keep track of time alive)
- [ ] Implement selection
- [ ] Implement crossover
- [ ] Implement mutation
- [ ] Implement replacement and elitism

# Novelty Search

Goal: replace the fitness function of the genetic algorithm with a function that
rewards novel behaviour instead.

How should I measure difference in behaviour though?
