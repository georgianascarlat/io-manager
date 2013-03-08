(load "io-manager.scm")
(require racket/string)
#|

A simple test module for io-manager.scm which will echo the contents of the
input files to the output files, paired as specified in standard input:
each line of stdin is made of two words: «input output» meaning that the
contents of «input» should be echoed to «output». If input is «@stdin» then
the content to be echoed is from the standard input. If output is «@stdout»
then content should be written to standard ouput instead of a file. Same is
true for the case where «@stderr» is in the ouput part.

Assume that the stdin description is valid: no «@stdout»/«@stderr» on the
input part and no «@stdin» on the output part. Also, no file appears both
in the input and output part.

|#

(define (read-everything promise init func)
  (let ((chunk (promise)))
    (if (eof-object? chunk)
        init
        (func chunk (read-everything promise init func)))))


(define (get-promise input fileName)
  (if (string=? "@stdin" fileName)
      (getStdIn input)
      (getInputFile  input fileName)))

(define (write-to output fileName string)
  (cond
    ((string=? "@stdout" fileName) (writeStdOut output string))
    ((string=? "@stderr" fileName) (writeStdErr output string))
    (else (writeOutputFile output fileName string))))

(define (my-echo input)
  (lambda (files output)
    (let ((fileNames (string-split files)))
      (if (= (length fileNames) 2)
          (write-to output (second fileNames) (read-everything (get-promise input (first fileNames)) "" string-append))
          output))))


(define (solve input output)
  (foldl (my-echo input) output (read-everything (getStdIn input) '() cons)))   

(define main (wrapIO solve read-line))

(main (vector->list (current-command-line-arguments)))
