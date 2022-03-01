;; In order to make a LispWorks Console image (in order to use the lispworks compiler/interpreter from the terminal)
;; call /Applications/path-to-lispworks --build path/to/this/file.lisp from a terminal.

(in-package "CL-USER")
(load-all-patches)
(save-image "/usr/local/bin/lispworks"
            :console t
            :environment nil
            :multiprocessing t)