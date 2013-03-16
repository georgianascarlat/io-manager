(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Creates an @input object that contains a promise to read from stdin 
and a list of lists of file names, ports and promises to read from them.

|#
(define (readInput readFunction fileNames)
  (cons (lambda ()  (readFunction))
        (map (lambda (fileName) 
               (list fileName (open-input-file fileName) (lambda (port) (lambda () (readFunction port)))))
               fileNames)))


#|

Modifies a element of @list given by @position using @function.

return: the list in which the element is modified

|#
(define (modifyOnPos position function lst)
  (append (take lst (- position 1)) (list (function (list-ref lst (- position 1)))) (drop lst position)))


#|

Appends the given string to the content of a file  from a
file entries list. 
If there is no entry for the given file name, then one will be
created.

return: modified file entries

|#
(define (appendToFile fileName string)
  (lambda (entries)
    (let* ((entry (assoc fileName entries))
           (buffer (if entry (cdr entry) "")))
      (cons (cons fileName (string-append buffer string)) (remove entry entries)))))


#|

Creates an empty @output object.

|#
(define initOutput
  '("" "" ()))


#|

Given a @fileEntry, representing a pair of a file name and content,
writes the content into the specified file.

If the file already existed, then the content is appended.

|#
(define (writeToFile fileEntry)
  (with-output-to-file (car fileEntry) (lambda () (display (cdr fileEntry))) 'append ))


#|

Writes the contents of the @output object to stdout, stderr
and to the needed files.
If the files already exist, they new content will be appended.

|#

(define (writeOutput output) 
  (display (first output) (current-output-port))
  (map writeToFile (third output))
  (display (second output) (current-error-port)))

#|

Closes all opened input files.

|#

(define (closeInputFiles input)
  (map (lambda (e) (if (not (port-closed? (second e))) (close-input-port (second e)))) (cdr input)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to be used by students
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| 

Used to obtain the contents of the standard input as given to the program.

return: a promise that offers a part of the contents of the standard input
at each evaluation, until there is no more data, when #<eof> is returned

|#
(define (getStdIn input)
  (car input))

#|

Used to obtain the contents of an input file.
It starts from the beginning of the file.

return:  a promise that offers a part of the contents of the input file
at each evaluation, until there is no more data, when #<eof> is returned

|#
(define (getInputFile input fileName)
  (let* ((entry  (assoc fileName (cdr input)))
         (port (second entry))
         (procedure (third entry)))
    (file-position port 0)
    (procedure port)))
    

#|

Appends text to the standard output. No newline is printed at the end,
the caller must handle it.
 
return: a new @output value, containing the appended text.

|#
(define (writeStdOut output string)
  (modifyOnPos 1 (lambda (x) (string-append x string)) output))


#|

Appends text to the standard error. No newline is printed at the end,
the caller must handle it.
 
return: a new @output value, containing the appended text.

|#
(define (writeStdErr output string)
  (modifyOnPos 2 (lambda (x) (string-append x string)) output))


#|

Appends to an output file. If the file does not exist in the @output
value (this program didn't yet write in it), it is created as a new one.
 
return: a new @output value, containing the appended text.

|#
(define (writeOutputFile output fileName string)
  (modifyOnPos 3 (appendToFile fileName string) output))


#|

Wraps a simple function (f @input @output) that returns @output in
order to simplify student's usage. 

The @readFunction must be specified. 
Examples of read functions: read, read-line, read-char, etc.

return: #<void>

|#
(define (wrapIO f readFunction)
  (lambda  (args)
    (let ((Input (readInput readFunction args))
          (Output initOutput))
      (writeOutput (f Input Output))
      (closeInputFiles Input)
      (flush-output))))

#|

Obtains the name of the file with index @index from
an @input object.
Indexing starts from 1.

return: file name

|#

(define (getFileName input index)
  (car (list-ref input index)))
