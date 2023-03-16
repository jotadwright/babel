;; Copyright AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :asdf)

(defsystem :inn
  :description "Interative Narrative Networks"
  :author "Remi van Trijp <remi.vantrijp@sony.com>" ;; Add your name if you co-develop :)
  :version "0.1"
  :depends-on (:cl-store :fcg :irl :graph-utils #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:module "classes"
                :serial t
                :components ((:file "integrative-narrative-network")
                             (:file "nodes")
                             (:file "edges")))
               (:module "visualization"
                :serial t
                :components ((:file "format-nodes")
                             (:file "format-edges")
                             (:file "draw-vis-network")))
               (:module "network-management"
                :serial t
                :components ((:file "update-network")))))
