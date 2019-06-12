(in-package :robot-interface)

(export '(speak hear))

(defgeneric speak (robot utterance &key speed)
  (:documentation "Make the robot say the utterance. Keyword argument can be used
   to control the speed of the text-to-speech. This is an integer in [0,100].
   This is a blocking call. Returns t when finished."))

(defgeneric hear (robot vocabulary)
  ;; cannot use 'listen' as it is a build in function on streams
  ;; this could use a timeout argument
  (:documentation "Make the robot listen for words in the vocabulary.
   Returns a single word; the last one recognised."))
