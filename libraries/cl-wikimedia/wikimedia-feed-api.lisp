;;;;; -----------------------------------------------------------------------------------
;;;;; (c) Sony Computer Science Laboratories Paris
;;;;;     Author: Remi van Trijp - remi.vantrijp@sony.com
;;;;; -----------------------------------------------------------------------------------

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

(in-package :cl-wikimedia)

(export '(wikimedia-feed-request
          wikimedia-featured wikimedia-on-this-day
          on-this-day-holidays on-this-day-events on-this-day-births on-this-day-deaths on-this-day-selected))

;; General Function
;; ----------------------------------------------------------------------------------
(defun wikimedia-feed-request (uri &key (lisp-format :alist))
  (let ((response-stream (drakma:http-request uri :content-type "application/json" :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format response-stream) :utf-8)
    (yason:parse response-stream :object-as lisp-format)))

;; Helper functions
;; ----------------------------------------------------------------------------------
(defun wiki-time (time-unit)
  "Get the current day, month or year formatted as string."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (declare (ignore sec min hour))
    (let ((time (case time-unit
                  (:day day)
                  (:month month)
                  (:year year)
                  (t
                   (error "Please specify :day :month or :year for wiki-time")))))
      (if (< time 10)
        (format nil "0~a" time)
        (format nil "~a" time)))))
;; (wiki-time :day)
;; (wiki-time :month)
;; (wiki-time :year)

;; Wikimedia-Featured + helper functions
;; ----------------------------------------------------------------------------------
(defun wikimedia-featured (&key (language "en")
                                (year (wiki-time :year))
                                (month (wiki-time :month))
                                (day (wiki-time :day))
                                (lisp-format :alist))
  "Get all information on the featured page of a given date. By default today."
  (let ((uri (format nil "https://api.wikimedia.org/feed/v1/wikipedia/~a/featured/~a/~a/~a"
                     language year month day)))
    (wikimedia-feed-request uri :lisp-format lisp-format)))

(defun wikimedia-featured-image (wikimedia-featured)
  (assert (hash-table-p wikimedia-featured))
  (gethash "image" wikimedia-featured))

(defun wikimedia-featured-mostread (wikimedia-featured)
  (assert (hash-table-p wikimedia-featured))
  (gethash "mostread" wikimedia-featured))

(defun wikimedia-featured-news (wikimedia-featured)
  (assert (hash-table-p wikimedia-featured))
  (gethash "news" wikimedia-featured))

(defun wikimedia-featured-onthisday (wikimedia-featured)
  (assert (hash-table-p wikimedia-featured))
  (gethash "onthisday" wikimedia-featured))

(defun wikimedia-featured-tfa (wikimedia-featured)
  (assert (hash-table-p wikimedia-featured))
  (gethash "tfa" wikimedia-featured))


;; Wikimedia-On-This-Day + helper functions
;; ----------------------------------------------------------------------------------
(defun wikimedia-on-this-day (&key (language "en")
                                   (month (wiki-time :month))
                                   (day (wiki-time :day))
                                   (lisp-format :alist))
  (let ((uri (format nil "https://api.wikimedia.org/feed/v1/wikipedia/~a/onthisday/all/~a/~a"
                     language month day)))
    (wikimedia-feed-request uri :lisp-format lisp-format)))

;; Accessor functions:
(defun on-this-day-holidays (on-this-day)
  (assert (hash-table-p on-this-day))
  (gethash "holidays" on-this-day))

(defun on-this-day-events (on-this-day)
  (assert (hash-table-p on-this-day))
  (gethash "events" on-this-day))

(defun on-this-day-births (on-this-day)
  (assert (hash-table-p on-this-day))
  (gethash "births" on-this-day))

(defun on-this-day-deaths (on-this-day)
  (assert (hash-table-p on-this-day))
  (gethash "deaths" on-this-day))

(defun on-this-day-selected (on-this-day)
  (assert (hash-table-p on-this-day))
  (gethash "selected" on-this-day))

#|
 (ql:quickload :web-interface)
 
 (defun hammer-time ()
   (web-interface::clear-page)
   (web-interface::add-element 
    `((div :id "HammerTime")
      ((h1) "Stop! Hammer Time")
      ((img :src "https://tenor.com/view/mc-hammer-stop-hammer-time-cant-touch-this-super-freak-hip-hop-gif-24612402.gif"))
      ((audio :id "hammeraudio" :src "https://www.myinstants.com/media/sounds/stop_1.mp3" :autoplay "autoplay"))
      ((script :type "text/javascript")
       "window.onload=function(){
             document.getElementById('hammeraudio').play();
       }"))))

 (hammer-time)
|#