sneakybeaky
==========

_Are we rushin' in - or are we goin' sneaky-beaky like?_

Inspired by Plomlompom's lightning talk [Roguelikes, and building one](http://events.ccc.de/congress/2014/wiki/Lightning:Roguelikes,_and_building_one), this is a roguelike in which you have to get from the starting position to the exit without getting killed. It's as simple as that.

Enemies are scattered throughout the map, patrolling randomly. Also, light sources are scattered throughout the map. If you step into the light and an enemy sees you, he will be alarmed. After seeing you multiple times, he will hunt you down!

### Screenshot(s)

![Screenshot of a typical game](https://raw.githubusercontent.com/pmiddend/sneakybeaky/master/img/screenshot.png)

### Installation

The game is written in Haskell, so you need the [Haskell Platform](https://www.haskell.org/platform/), especially cabal. Using that, it's simply

    cabal install --only-dependencies
    cabal run

### Controls

    hjkl => horizontal movement (left, down, up, right)
    yubn => diagonal movement (left-top, right-top, left-bottom, right-bottom)
    q    => quit

### Known issues

  - You might be spawned inside an enemy and instantly lose the game
  - You might be spawned next to an enemy and lose after your first move
  - You might be spawned inside a bright light source, so you are seen by the enemies.t
