Triangula app enumerates all possible solutions that can happen in "Triangula" game and shows you the path of the maximum likelihood of win.

##Game rules
!!!!! note to myself: check the rules for correctness

1. There are two players and one board (say, 3 by 3)
2. Players connect dots with edges. Only certain types of edges are considered valid - for example, 
2.1. no other vertices should lie on the edge
2.2. edge should not cross any other edges
3. If a player draws an edge which forms a triangle, the area of this triangle is marked as belonging to this player
4. The goal of the game is to cover bigger area than your opponent. Ties are possible - when both players cover 50% of the board.

##How Triangula works
Obviously there are many ways to win (or to lose) the game, with some moves being "better" and some moves being "worse" - in a sense that a good move increases your chances to win the board, and vice versa. What Triangula application does is it generates all possible solutions and sums up how many winning/losing boards every move produces.

It's very easy to generate all solutions. A somewhat harder task is to store all of them in memory. Even a simple tic-tac-toe game tree has 9! nodes, and Triangula is much more complex than that. The key here is to use pruning.

For example, let suppose we have an empty 3-by-3 board with vertices from (1,1) to (3,3). We can generate two child boards - one by adding an edge (1,2)->(2, 1), and another one by adding an edge (2,3)->(3,2). They look different, by one board can be transformed into another by applying conformal mapping - in this case you just have to rotate one of the boards by 180 degrees. This is how you can remove one of the boards from the set and simplify your calculations. It's not always that simple though. You have to think about triangles as well - if two boards have the same edges but form triangles of different color - such boards are obviously different and should not be merged.

7 different mappings
