# Puzzle-solving via backtracking

- Kakuro instances must be hard-coded into the program.
- Wolkenkratzer restrictions can be read directly from a descriptor file, eg:

```shell
$ guile src/wolkenkratzer.scm < res/wolkenkratzer-6x6-janko-44.txt
$ guile src/kakuro.scm
```

Code is compatible with Guile, so other implementations may not be supported.

The generic backtracking procedure was also tested with depth-first search on a binary tree and by solving the N-Queens problem.
