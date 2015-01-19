# parity-solver
Parity is a recently published game in which you try to get the numbers in all squares of the playing-board equal, you can increase the number in a particular square by moving the "selector" onto it (using the arrow keys). You can find the game at: http://www.abefehr.com/parity/

This Haskell program models the game and, via the Data.Graph.AStar package, tries to find the optimal path through it. The graph it searches is composed of the individual GameStates (which are made up of the current position of the "selector" and the board, which in turn can be either a StdBoard or a Board containing both black & white squares) as the vertices and up, down, left and right moves as the edges.

Feedback on performance and style improvements are greatly appreciated!
