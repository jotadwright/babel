(in-package :clevr-dialog-grammar)

(export '(;;all object properties
          cube sphere cylinder thing
          gray red blue green brown purple cyan yellow violet 
          yellow-bg white-bg salmon-bg silver-bg cyan-bg
          left right behind front center above below 2D-right 2D-left
          flat stroke
          small large rubber metal
          yes no
          zero one two three four five six seven eight nine ten
          color shape material size relation attribute none number bgcolor digit style 
          number-category boolean-category shape-category color-category material-category size-category style-category spatial-relation-category attribute-category digit-category bgcolor-category 2D-relation-category
          ;; primitives
          count-objects exist-or-count
          exist extreme-relate
          find-in-context filter-by-attribute
          get-penultimate-topic get-last-topic
          get-context get-memory
          get-last-attribute-category immediate-relate
          more-than-1 query
          relate set-diff unique select-one
          segment-scene
          understand
          ))