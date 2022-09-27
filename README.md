# eye-math

Lisp package to define some useful functions for performing simple optical calculations such as determining your prescription based on how far away you can read 20/20 or based on when you observe first blur as well as utilities to tell you how far away your glasses are good for.

Designed and implemented during my EndMyopia journey to trivialize the calculations.

## Install

Not a part of quicklisp. If using quicklisp, add to you quicklisp/local-projects directory, ```cd /path/to/quicklisp/local-projects/; git clone https://github.com/afranson/eye-math```, then use ```(ql:quickload :eye-math)``` inside a repl (slime, sly, command line), then run ```(em:eye-diagnostics)```, read the information provided, then use to your heart's content.

I use this functionality inside a basic roswell script like,
```
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp(ql:quickload '(#:eye-math) :silent t)
  )

(defpackage :ros.script.eyes.3873029596
  (:use :cl
        :eye-math))
(in-package :ros.script.eyes.3873029596)

(defun main (&rest argv)
  (apply #'eye-diagnostics argv))

```
; just make the file executable (after installing roswell of course) and you'll be able to access this functionality from the command line.

Alternatively, you can add to your ASDF source directory (wherever you configured it to be) and use ```(asdf:load-system :eye-math)``` in your favority REPL environment.

## License

GPLv3

