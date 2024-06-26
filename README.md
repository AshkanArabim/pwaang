# PWAANG
A board game written entirely in Haskell.

## Background
In Spring 2024, in my "Programming Language Concepts" class, we had to make a game called PWAANG (Professor Ward's Amazingly Awesome New Game). The original assignment just required us to satisfy a few test-cases, but I decided to implement the full game after the end of the semester to flex dem skillz (and learn some extra Haskell in the process).

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
  - If the direction determined by the die is impossible given the current position of the player's beans, the turn is skipped.
  - If a move is possible, the player will enter the location of the bean that they want to move (see [rules](#rules)).
  - After the move is made, if the opponent has no beans left, the player wins. If not, game continues.

## How to set up & play
1. Install Haskell and Stack (varies a lot depending on your OS; see [this](https://docs.haskellstack.org/en/stable/install_and_upgrade/) guide)
2. Clone this repo.
3. `cd` into the repo folder.
4. Run `stack build`
   1. You may have to isntall some Haskell modules using Stack in this phase. 
5. Run `stack exec pwaang-extended-exe`
6. Start playing!

## Screenshots
![Mid-game screenshot](https://github.com/AshkanArabim/pwaang-extended/assets/71609332/a9daeba6-282d-4304-804d-96ac062299ca)
![Game over screenshot](https://github.com/AshkanArabim/pwaang-extended/assets/71609332/52f6f944-c487-42bd-a43c-48266c1c87c2)


## Acknowledgements
Assignment idea and part of the implementation by [Dr. Nigel Ward](https://scholar.google.com/citations?user=ncnkwCMAAAAJ&hl), professor at the University of Texas at El Paso.

I'll take down this project if it becomes the subject of plagiarism for school assignments. If you're an instructor, just shoot me an email at ashkan.arabim@gmail.com and I'll do it.
