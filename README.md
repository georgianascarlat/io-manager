io-manager
==========

This is a framework which simplifies the use of IO operations in Scheme.

There is an example that illustrates what one should do in order to use this framework:

- load framework: (load "io-manager.scm")
- wrap the function that uses IO operations: (define main (wrapIO solve read-line))


Example
=======

The repository contains an example called simple-example.scm. 
In order to run the example make sure that it's name is written inside loader.scm:
(define-runtime-path prog "simple-example.scm")

Also, make sure to call the main function using the command line arguments list as 
parameter inside simple-example.scm: 
(main (vector->list (current-command-line-arguments)))

Run make to build it and then give as arguments the name of the files from which you want 
to read. Finish stdin with EOF (Ctrl + D).

Example:

-./simple-example Makefile loader.scm 
-loader.scm ll 
-Makefile @stdout
-loader.scm @stderr
-@stdin ss
-now writing to file ss
