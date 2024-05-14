# PWAANG
Extended version of a Haskell class assignment.

## Background
In Spring 2024, in my "Programming Language Concepts" class, we had to make a game called PWAANG (Professor Ward's Amazingly Awesome New Game). The original assignment just required us to satisfy a few test-cases, but I decided to implement the full game after the end of the semester to flex dem skillz.

## Rules
- This is a two-player game.
- The game is played on a 3x3 grid, where positions are numbered like this (in the codebase,
and in case you want to refer to a cell):
```
6 | 7 | 8
---------
3 | 4 | 5
---------
0 | 1 | 2
```
- Each player starts with three beans (white or pinto)
- At the beginning of the game, the beans are arranged like this:
```
w | w | w
---------
  |   |
---------
p | p | p
```
- On each round, players will roll a die, based on which they should move one of their beans to a new position 
(and hopefully capture an enemy bean). Players can capture their own beans, and are forced to do so if it's
their only option. If there are no moves available to the player (e.g. all are out of bounds), the turn is
skipped.
- Last standing player wins.
- This game uses an eight-sided die, where each number represents a bean moving direction (see the shape below):
  - 1: east
  - 2: northeast
  - 3: north
  - 4: northwest
  - 5: west
  - 6: southwest
  - 7: south
  - 8: southeast
```
        N

        3
     4     2
W   5   .   1   E
     6     8
        7

        S
```
- There are three things that can happen after each person rolls their die:
  - If the direction determined by the die is impossible given the current position of beans, the turn is skipped.
  - If opponent has no beans left after the player makes a move, the player wins.
  - If none of the above happen, the player gets to choose a bean to move, and the game continues.

## How to set up & play
Stay tuned...

## Acknowledgements
Assignment idea by (Dr. Nigel Ward)[https://scholar.google.com/citations?user=ncnkwCMAAAAJ&hl], professor at the University of Texas at El Paso.

I'll take down this project if it becomes the subject of plagiarism for school assignments. If you're an instructor, just shoot me an email at ashkan.arabim@gmail.com and I'll do it.
