(in-package #:type-30-display)

(defconstant +dmask+ #o777777)
(defconstant +ov+ #o1000000)

(defparameter *test-word* '(7 8 8 8 3 2))
(defparameter *primera-vez* t)

(declaim (inline 18bit->num add18 cma18 sar18 sub18))
(declaim (ftype (function (fixnum) fixnum) cma18 18bit->num))

(defun cma18 (ac)
  (declare (optimize (speed 3))
           (type fixnum ac))
  (logxor +dmask+ ac))

(declaim (ftype (function (fixnum fixnum) fixnum) add18 sub18 sar18))

(defun add18 (ac mb)
  (declare (optimize (speed 3))
           (type fixnum ac mb))
  (incf ac mb)
  (cond ((not (zerop (logand ac +ov+)))
         (logand (1+ ac) +dmask+))
        ((= ac +dmask+) 0)
        (t ac)))

(defun sub18 (ac mb)
  (declare (optimize (speed 3))
           (type fixnum ac mb))
  (setf ac (cma18 ac))
  (incf ac mb)
  (when (not (zerop (logand ac +ov+)))
    (setf ac (logand (1+ ac) +dmask+)))
  (cma18 ac))

(declaim (type (simple-array fixnum) *shift-masks-hi*))
(defparameter *shift-masks-hi*
  (make-array 19 :element-type 'fixnum
                 :initial-contents (loop for sh from 0 to 18
                                         collect (ash (1- (ash 1 sh))
                                                      (- 18 sh)))))

(defun sar18 (ac sh)
  (declare (optimize (speed 3)))
  (assert (<= 0 sh 18))
  (let ((bits (ash ac (- sh))))
    (if (zerop (ldb (byte 1 17) ac))
        bits
        (logior (aref *shift-masks-hi* sh) bits))))

(defun 18bit->num (18bit)
  (declare (optimize (speed 3)) (type fixnum 18bit))
  (if (zerop (ldb (byte 1 17) 18bit))
      (ldb (byte 17 0) 18bit)
      (- (ldb (byte 17 0) (cma18 18bit)))))

;;;; '(4 4 8 8 3 3)
;;;; '(7 8 8 8 3 2)

(defun gen-minskytron-pars (&optional data values)
  (if data
      (append (subseq data 0 6)
              (or values (list #o757777 0 0 #o040000 #o020000 0)))
      (let ((data (if *primera-vez*
                      (prog1 *test-word*
                        (setf *primera-vez* nil))
                      (loop repeat 6 collect (1+ (random 8))))))
        (format *debug-io* "TEST WORD: ~S~%" data)
        (append data
                (or values (list #o757777 0 0 #o040000 #o020000 0))))))

(defun minskytron-draw-point (stream x y ink &optional (radio 1.25))
  (declare (optimize (speed 3))
           (type fixnum x y))
  ;;(format *debug-io* "~5o ~5o~%" x y)
  (let* ((x (ash (18bit->num x) -8))
         (y (ash (18bit->num y) -8))
         (px (+ *width/2* x))
         (py (- *height/2* y)))
    (draw-circle* stream px py radio
                  :filled t
                  :ink ink
                  :line-thickness 0.5)))

(defun gen-minskytron (stream ink data &optional (max-iter 1024))
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (type fixnum max-iter))
  (flet ((draw-point (x y)
           (minskytron-draw-point stream x y ink)))
    (loop with (sh0 sh1 sh2 sh3 sh4 sh5 xa ya xb yb xc yc) = data
          with frame = (list)
          for i fixnum from 0 below (or max-iter 2048)
          do (setf ya (add18 ya (sar18 (add18 xa xb) sh0))
                   xa (sub18 xa (sar18 (sub18 ya yb) sh1))
                   yb (add18 yb (sar18 (sub18 xb xc) sh2))
                   xb (sub18 xb (sar18 (sub18 yb yc) sh3))
                   yc (add18 yc (sar18 (sub18 xc xa) sh4))
                   xc (sub18 xc (sar18 (sub18 yc ya) sh5)))
             (draw-point xa ya)
             (draw-point xb yb)
             (draw-point xc yc)
             (push (cons xa ya) frame)
             (push (cons xb yb) frame)
             (push (cons xc yc) frame)
          finally (return (values xa ya xb yb xc yc frame)))))

(defun draw-frame (pixmap frame ink)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop for punto in frame
        do (minskytron-draw-point pixmap (car punto) (cdr punto) ink 1.75)))

(defparameter *num-frames* 48)

(defparameter *colors* (reverse
                        (loop for c from 1.0d0 downto 0.0d0 by (/ 1.0d0 *num-frames*)
                              collect (clim:make-rgb-color 0.0000 c c))))

(defun minskytron (pane data width height)
  (declare (optimize (speed 3))
           (ignore data))
  (let ((stream (type-30-medium-1 pane)))
    (when (zerop (type-30-frame-number pane))
      (draw-rectangle* stream 0 0 width height :filled t :ink +black+))
    (loop for frame in (reverse (type-30-frames pane))
          for i from 1 for color in *colors*
          do (draw-frame stream frame color))
    (multiple-value-bind (xa ya xb yb xc yc frame)
        (gen-minskytron stream +white+ (type-30-data pane) 24)
      (when (= (length (type-30-frames pane)) *num-frames*)
        (setf (type-30-frames pane) (butlast (type-30-frames pane))))
      (push frame (type-30-frames pane))
      (medium-copy-area stream 0 0 width height (type-30-medium-0 pane) 0 0)
      (setf (type-30-data pane) (gen-minskytron-pars (type-30-data pane)
                                                     (list xa ya xb yb xc yc))))))
;;;;
