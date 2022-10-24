(in-package :clevr-world)

(export '(;;all object properties
          cube sphere cylinder thing
          gray red blue green brown purple cyan yellow
          left right behind front
          small large rubber metal
          yes no
          ;; primitives
          count! equal-integer less-than greater-than
          equal? exist filter get-context intersect
          union! query relate same unique
          clevr-context))