# term-monopoly

A [CS 3110](https://www.cs.cornell.edu/courses/cs3110/2017fa/) project by [Anmol Kabra](https://anmolkabra.com), [Jehron Petty](https://github.com/JehronPett), [Jared Lim](https://github.com/JunyoungLim), and [Marina Chang](mailto:mpc84@cornell.edu).

Special thanks to [CS 3110 Fall 2017](https://www.cs.cornell.edu/courses/cs3110/2017fa/) course staff for the course.

## What is this?

term-monopoly is a terminal-based framework in OCaml for the Monopoly game, using which upto 8 users can play Monopoly. We also provide 3 AI agents with differing difficulty, and it is possible for multiple AI agents to play with each other. Developers could program new AIs based on the framework, and edit the source code to challenge the players.

Some monopoly boards (classic.json and 3110.json) are provided to get you started on the game.

Salient features:
- Terminal-based framework to allow customizable Monopoly boards.
- The terminal GUI provide a user-friendly interface to actions and views in the game.
- 3 AI agents with AI3 as the most advanced game player.
- Modular structure for developers to add more AI agents.

## How to play?

**Tested with OPAM 1.2.2 and OCaml 4.05.0 compiler on Linux and OSX platforms.**

### To install dependencies:
Install OPAM version 1.2.2 and OCaml 4.05.0 compiler as described [here](http://www.cs.cornell.edu/courses/cs3110/2017fa/install.html). The game _might_ work on other versions as well. Then install the third-party dependencies for the game (most should have been installed by the OCaml install as described in that page):
```bash
make install-dep
```

### To compile and run the game:
From the directory containing all the files,
```bash
make
# or
make game
```

### Instructions:
The game is played by pressing the keys corresponding
to the actions displayed in the GUI.
The key presses are case sensitive,
for example a capital 'N' is entered by pressing
the 'n' and 'shift' keys simultaneously.
Windows with scroll bars can be scrolled Using
the page up/page down keys. In some terminals, scrolling
also works using a mouse.

Upon starting a new game, the GUI will request a game file.
The available full game files to play are:
- classic.json – (based on the American monopoly board)
- 3110.json – (based on the [CS 3110 Fall 2017](https://www.cs.cornell.edu/courses/cs3110/2017fa/) schedule)

The following small testing files are also available:
- test_board.json – (minimal file containing one of each type)
- test_board_2.json - (slightly extended version of test_board)
- test_board_railroads.json - (file with only railroads and jail)
- test_board_jail.json - (file with only spaces and cards related to jail)

The players for the game can be selected from the following
types of controllers:
- Human Player - uses key inputs to determine which actions to take
- AI1 - uses a very simple AI to determine which actions to take
- AI2 - uses a smarter AI to determine which actions to take
- AI3 - uses a complex AI to determine which actions to take

## Makefile usage

1. `make` or `make game` -> Make and run the game.
2. `make check` -> Check the OPAM env and types, and compile the game.
3. `make clean` -> Clean the `_build/` directory.
4. `make test` -> Make the test file and run it.
5. `make sim` -> Simulate the game using AI
6. `make install-dep` -> Install dependencies for this project.