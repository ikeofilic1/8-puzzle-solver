# Expense 8-puzzle Solver
A state space search agent built to solve a modifed version of the 8 puzzle problem called the Expense 8 puzzle problem. The task is still to take a 3X3 grid on which 8 tiles have been placed, where you can only move one tile at a time to an adjacent location (as long as it is blank) and figure out the order in which to move the tiles to get it to a desired configuration. The modification is that the number on the tile now also represents the cost of moving that tile (i.e. moving the tile marked 6 costs 6)

## Structure
There are 3 files, two of which I copied from an online repository: 
https://github.com/oconnore/queues (that is priority-queue.lisp and queue.lisp)

I did not have enough time to implement my own priority queue in Lisp this is why I used the queues library.
Also, you would notice that I chose to include the library locally instead of using Lisp's module system. This is because of 2 reasons.
1. I edited the source code for the library because I did not like how the print-queue function displayed the node structures.
2. This makes the code more portable and stable in the future


The main script has been made an executable file. However, an sbcl implementation of the lisp compiler must be present on
the system.

In the main file, I have described all the functions by name. ucs is called ucs, DFS, dfs and so on.
This was the easiest way to do it in Lisp. Anyways, there are no error checking or anything.
This means that anything that is not in the right format will crash the code. 

After all the function definitions and descriptions we see the entry point called `main`. It is called at the end by '(main)'

## Running the Code

To Install SBCL (On Debian flavor of Linux):
```shell
$ sudo apt install sbcl
```

Then run the script as 
```shell
$ ./expense_8.lisp <start-file> <goal-file> <method> <dump>
```

## Notes
Please note that dump is a lazy variable such that running `expense_8.lisp start.txt goal.txt ucs haha` will cause the script to dump a trace file
If you do not wish to dump a file, do not pass any argument in place of <dump>


## Future plans
I plan to extend this project by adding a window that allows the user to view the puzzle being solved in real time (as tiles in a grid).
