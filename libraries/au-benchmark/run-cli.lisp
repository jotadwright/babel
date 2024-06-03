(ql:quickload :au-benchmark)
(in-package :au-benchmark)

#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))