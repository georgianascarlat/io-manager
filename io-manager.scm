(require racket/list)


(define (file-read readFunction port)
  (lambda ()
    (let ((data (readFunction port)))
      (if (eof-object? data)
          (close-input-port port)
          data))))

(define (my-readFile readFunction)
  (lambda (fileName)
    (cons fileName (file-read readFunction (open-input-file fileName)))))

(define (modifyOnPos position function lst)
  (append (take lst (- position 1)) (list (function (list-ref lst (- position 1)))) (drop lst position)))

(define (appendToFile fileName string)
  (lambda (entries)
    (let* ((entry (assoc fileName entries))
           (buffer (if entry (cdr entry) "")))
      (cons (cons fileName (string-append buffer string)) (remove entry entries)))))


(define (writeToFile fileEntry)
  (if (file-exists? (car fileEntry)) (delete-file (car fileEntry)))
  (with-output-to-file (car fileEntry) (lambda () (write (cdr fileEntry))) ))





(define (getStdIn Input)
  (car Input))

(define (getInputFile Input fileName)
  (cdr (assoc fileName (cdr Input))))


(define (writeStdOut output string)
  (modifyOnPos 1 (lambda (x) (string-append x string)) output))

(define (writeStdErr output string)
  (modifyOnPos 2 (lambda (x) (string-append x string)) output))

(define (writeOutputFile output fileName string)
  (modifyOnPos 3 (appendToFile fileName string) output))


(define initOutput
  '("" "" ()))

(define (readInput readFunction fileNames)
  (cons (lambda ()  (readFunction))
        (map (my-readFile readFunction) fileNames)))

(define (writeOutput output) 
  (write (first output) (current-output-port))
  (write (second output) (current-error-port))
  (map writeToFile (third output))
  initOutput)

(define (wrapIO f readFunction)
  (lambda (args)
    (let ((Input (readInput readFunction args))
          (Output initOutput))
      (writeOutput (f Input Output)))))




(define (solve input output)
  (let* ((in (getInputFile input "in.txt"))
         (stdin (getStdIn input)))
    (begin
      ;(display (stdin))
      ;(display (stdin))      
      ;(display (in))
      ;(display (in))
      ;(display (in))
      ;(display (in))
      ;(display (in))
      ;(writeStdOut output "Stdout!")
      (writeOutputFile (writeOutputFile (writeStdOut (writeStdErr output (in))  (in)) "out.txt" (stdin))  "out.txt" (stdin))        
      ))) 

(define main (wrapIO solve read-line))
(main '("in.txt"))
