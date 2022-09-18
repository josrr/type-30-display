(in-package #:type-30-display)

(declaim (type fixnum *width* *height* *width/2* *height/2* *pausa*))
(defparameter *width* 1024)
(defparameter *height* 1024)
(defparameter *width/2* (/ *width* 2))
(defparameter *height/2* (/ *height* 2))
(defparameter *pause* (floor 1000000000 120))
