;; ########################################################################
;;                                                                        #
;; Extracting a technical manual for Common Lisp ASDF packages with CLDOC #
;;                                                                        #
;; ########################################################################

;; # Step 1: load the cl-doc package
(ql:quickload :cldoc)
(in-package :cldoc)


;; # Step 2: load the package that you want to document
;; # Step 3: Call the extract-documentation-fcg function with html-driver,
;;           target-directory, asdf system and table-of-contents-title

;; # Step 4: All html files and index files are now in your target directory

;; # IMPORTANT: if the function throws an error (e.g. unbounded variable), click
;;              on the option of specifying a value manually (e.g. nil) and continue


;; #################################
;; Documenting the Babel 2 systems #
;; #################################

;; (ql:quickload :fcg)
(extract-documentation-fcg 'html
                           "~/Desktop/tech-doc/fcg"
                           (asdf/system:find-system :fcg)
                           :table-of-contents-title "Fluid Construction Grammar (FCG)")

;; (ql:quickload :irl)
(extract-documentation-fcg
      'cludg:html 
      "~/Desktop/tech-doc/irl" 
     (asdf:find-system :irl) 
      :table-of-contents-title "Incremental Recruitment Language (IRL)")

;; (ql:quickload :web-interface)
(extract-documentation-fcg
      'cludg:html 
      "~/Desktop/tech-doc/web-interface" 
      (asdf:find-system :web-interface) 
      :table-of-contents-title "Web Interface")

;; (ql:quickload :action-behavior-framework)
(extract-documentation-fcg
      'cludg:html 
      "~/Desktop/tech-doc/action-behavior-framework" 
     (asdf:find-system :action-behavior-framework) 
      :table-of-contents-title "Action-Behavior Framework")

;; (ql:quickload :experiment-framework)
(extract-documentation-fcg 
      'cludg:html 
      "~/Desktop/tech-doc/experiment-framework" 
     (asdf:find-system :experiment-framework) 
      :table-of-contents-title "Experiment Framework")

;; (ql:quickload :meta-layer-learning)
(extract-documentation-fcg
      'cludg:html 
      "~/Desktop/tech-doc/meta-layer-learning" 
     (asdf:find-system :meta-layer-learning) 
      :table-of-contents-title "Meta-Layer Learning")

;; (ql:quickload :monitors)
(extract-documentation-fcg
      'cludg:html 
      "~/Desktop/tech-doc/monitors" 
     (asdf:find-system :monitors) 
      :table-of-contents-title "Monitors")

;; (ql:quickload :tasks-and-processes)
(extract-documentation-fcg
      'cludg:html 
      "~/Desktop/tech-doc/tasks-and-processes" 
     (asdf:find-system :tasks-and-processes) 
      :table-of-contents-title "Tasks and Processes")

;; (ql:quickload :utils)
(extract-documentation-fcg
      'cludg:html 
      "~/Desktop/tech-doc/utils" 
     (asdf:find-system :utils) 
      :table-of-contents-title "Utils")

;; (ql:quickload:plot-raw-data)
(extract-documentation-fcg
      'cludg:html 
      "~/Desktop/tech-doc/plot-raw-data" 
     (asdf:find-system :plot-raw-data) 
      :table-of-contents-title "Plot Raw Data")

;; ############################
;; Add to fcg-net.org server: #
;; ############################

;; Local:
;; scp -r ~/Desktop/tech-doc paul@fcg-net.org:/home/paul/

;; On server:
;; mv tech-doc /var/www/wordpress/
