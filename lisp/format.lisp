#!/usr/bin/sbcl --script

 (require :uiop)

 ;Verify 1 command line argument
 (when (/= (length uiop:*command-line-arguments*) 1)
  (progn (write-line "Error there must be exactly one command line argument") (quit)))



;returns a list of all the words in the input file
(defun get-words ()
  (with-open-file (stream (car uiop:*command-line-arguments* ))
    (loop for line = (read-line stream nil)
          while line
          append (uiop:split-string line :separator " ")
    )
  )
)

;returns the input word stripped of all numbers
(defun removeNumbers (word) 
  (remove #\9 (remove #\8 (remove #\7 (remove #\6 (remove #\5 (remove #\4 (remove #\3 (remove #\2 (remove #\1 (remove #\0 word))))))))))
)

;initial values for the longest and shortest lines
(defvar maxLine "")
(defvar maxLineNum -1)
(defvar minLine "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA") ;60 chars
(defvar minLineNum -1)

;recursively processes the list of words
(defun processList (words currLine lineNum)
  (let ((word (removeNumbers (car words))))     ;declare local variable word to be the first word with all numbers removed
  (cond
    ((not words)                                ;if there are no more words in the list
    (format t "~8@a  ~a~&" lineNum currLine)    ;write the current line
    (when (<= (length currLine) (length minLine)) (progn (setq minLine currLine) (setq minLineNum lineNum))) ;update minLine
    (when (>= (length currLine) (length maxLine)) (progn (setq maxLine currLine) (setq maxLineNum lineNum)))) ;update maxLine

    ((= (length currLine) 0)                  ;else if currLine is the empty string
    (processList (cdr words) word lineNum))    ;call function again with currLine set to the first word

    ((= (length word) 0)                           ;else if word is the empty string
    (processList (cdr words) currLine lineNum))    ;call function again on the next word; we're ignoring this word

    (( <= ( + (+ (length currLine) (length word)) 1) 60)                           ;else if currLine + first word + 1 <= 60
    (processList (cdr words) (concatenate 'string currLine " " word) lineNum ))    ;add first word to currLine and call function again

    (t                             ;else
    (format t "~8@a  ~a~&" lineNum currLine)
    (when (<= (length currLine) (length minLine)) (progn (setq minLine currLine) (setq minLineNum lineNum))) ;update minLine
    (when (>= (length currLine) (length maxLine)) (progn (setq maxLine currLine) (setq maxLineNum lineNum))) ;update maxLine
    (processList (cdr words) word (+ lineNum 1))  ;call function again with currLine set to the first word and currLineNum incremented
    )
  )
)
)

(processList (get-words) "" 1) ;this one function call performs almost the entire program!
(terpri)

;display longest and shortest lines
(format t "~6a ~11a  ~a~&" "LONG" maxLineNum maxLine)
(format t "~6a ~11a  ~a~&" "SHORT" minLineNum minLine)
