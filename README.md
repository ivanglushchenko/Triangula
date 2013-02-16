Triangula app enumerates all possible solutions that can happen in "Triangula" game and shows you the path of the maximum likelihood of win.

##Game rules

1. There are two players and one board (say, 3 by 3)
2. Players alternate in connecting the dots with edges. Only certain types of edges are considered valid - for example, 
a. no other vertices should lie on the edge
b. the edge should not cross any other edge
3. If a newly placed edge forms a triangle, the area of this triangle is marked as belonging to the player who put the edge, and he is allowed to make another turn.
4. The goal of the game is to cover bigger area than your opponent. Ties are possible - when both players cover 50% of the board.

##How it works
Obviously there are many ways to win (or to lose) the game, with some moves being "better" and some moves being "worse" - in a sense that a good move increases your chances to win the board, and vice versa. What Triangula application does is it generates all possible solutions and sums up how many winning/losing boards every move produces.

It's very easy to generate all solutions. A somewhat harder task is to store all of them in memory. Even a simple tic-tac-toe game tree has 9! nodes, and Triangula is much more complex than that. The key here is to use pruning.

For example, let's suppose we have an empty 3-by-3 board with vertices (1,1) to (3,3). We can generate two child boards - one by adding an edge (1,2)->(2, 1), and another one by adding an edge (2,3)->(3,2). They look different, but one board can be transformed into another by applying conformal mapping - in this case you just have to rotate one of the boards by 180 degrees. This is how you can remove one of the boards from the set and simplify your calculations. It's not always that simple though. If two boards have the same edges but form triangles of different color - such boards are obviously different and should not be merged.

##How it looks
There are only 4 unique starting boards in a 3x3 game:

![](/images/t1.png)

[Some] Boards after 6 turns:

![](/images/t2.png)

Player 1 won this partucular game:

![](/images/t3.png)
