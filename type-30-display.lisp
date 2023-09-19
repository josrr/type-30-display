(in-package #:type-30-display)

(defun animate (frame)
  (lambda ()
    (when (member (frame-state frame) '(:enabled :shrunk))
      (loop with time = (local-time:now)
            and pane = (find-pane-named frame 'display-pane)
            with width = (bounding-rectangle-width (sheet-region pane))
            and height = (bounding-rectangle-height (sheet-region pane))
            while (bt:with-lock-held ((type-30-lock frame))
                    (null (type-30-stop pane)))
            if (bt:with-lock-held ((type-30-lock frame))
                 (and (type-30-playing pane) (type-30-animation-func pane)))
              do (when (type-30-animation-func pane)
                   (funcall (type-30-animation-func pane) pane (type-30-data pane) width height)
                   ;;(setf (pane-needs-redisplay pane) t)
                   (queue-event pane (make-instance 'window-repaint-event :region +everywhere+ :sheet pane)))
            else do (bt:thread-yield) end
            do (setf time (local-time:timestamp+ time *pause* :nsec))
               (sleep (max 0.0d0
                           (the double-float
                                (local-time:timestamp-difference
                                 time (local-time:now)))))))))

(defparameter *type-30-display* nil)

(defun start ()
  (let ((frame (make-application-frame 'type-30-display)))
    (setf *type-30-display* frame)
    (run-frame-top-level frame)
    (let ((hilo (type-30-animation-thread frame)))
      (when (and hilo (bt:thread-alive-p hilo) (bt:destroy-thread hilo)))))
  :name "Type 30 CRT")

(defun salir ()
  #+sbcl (sb-ext:quit)
  #+ecl (ext:quit)
  #+clisp (ext:exit)
  #+ccl (ccl:quit)
  #-(or sbcl ecl clisp ccl) (cl-user::quit))

(defun main (&rest arguments)
  (declare (ignore arguments))
  (setf *primera-vez* t)
  (bt:join-thread (bt:make-thread #'start :name "type-30-display"))
  (unless (find :swank *features*)
    (salir)))

(defclass display-pane (clim-stream-pane clime:never-repaint-background-mixin)
  ((pixmap-0 :initform nil :accessor type-30-pixmap-0)
   (pixmap-1 :initform nil :accessor type-30-pixmap-1)
   (medium-0 :initform nil :accessor type-30-medium-0)
   (medium-1 :initform nil :accessor type-30-medium-1)
   (frame-number :initform 1 :accessor type-30-frame-number)
   (animation-func :initarg :animation-func :initform nil :accessor type-30-animation-func)
   (data :initform nil :accessor type-30-data)
   (frames :initform nil :accessor type-30-frames)
   (playing :initform nil :accessor type-30-playing)
   (stop :initform nil :accessor type-30-stop)))

(defmethod initialize-instance :after ((pane display-pane) &key contents)
  (declare (ignore contents))
  (setf (stream-recording-p pane) nil)
  t)

(defmethod compose-space ((pane display-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :min-width *width*
                          :min-height *height*
                          :width *width*
                          :height *height*
                          :max-width *width*
                          :max-height *height*))

(defmethod handle-repaint ((pane display-pane) region)
  (bt:with-lock-held ((type-30-lock *application-frame*))
    (when (type-30-pixmap-0 pane)
      (copy-from-pixmap (type-30-pixmap-0 pane) 0 0 *width* *height* pane 0 0)))
  t)

#|(defun draw (frame pane)
  (declare (ignore frame))
  (when (type-30-pixmap-0 pane)
    (bt:with-lock-held ((type-30-lock *application-frame*))
      (copy-from-pixmap (type-30-pixmap-0 pane) 0 0 *width* *height* pane 0 0))))|#

(defun draw-switch (pane x0 y0 wnibble h idx-bit bit)
  (let* ((wbit (/ wnibble 3))
         (wbit/2 (/ wbit 2))
         (rbit (/ wbit 5))
         (hsw/2 (/ h 10))
         (x (+ x0 (* wbit (- 2 idx-bit))))
         (xbit (+ x wbit/2))
         (ybit (+ y0 (* (1+ (* -2 bit)) hsw/2))))
    (draw-rectangle* pane (- xbit 1)  (+ y0 hsw/2) (+ xbit 4) (- y0 hsw/2)
                     :filled t :line-thickness 0 :ink +gray50+)
    (draw-rectangle* pane (+ xbit 1) (+ y0 hsw/2) (+ xbit 4) (- y0 hsw/2)
                     :filled t :line-thickness 0 :ink +gray65+)
    (draw-line* pane (- xbit 3) (+ y0 hsw/2) (- xbit 4) (- y0 hsw/2)
                :line-thickness 2 :ink +gray40+)
    (draw-circle* pane xbit ybit rbit :filled t :ink +gray50+)
    (draw-circle* pane xbit ybit rbit :filled nil
                                      :line-thickness 3 :ink +gray40+)
    (draw-circle* pane (+ xbit (/ rbit 3.75)) (- ybit (/ rbit 3.75)) (/ rbit 1.8)
                  :filled t :ink +gray53+)
    (draw-circle* pane (+ xbit (/ rbit 3)) (- ybit (/ rbit 3)) (/ rbit 3)
                  :filled t :ink +gray58+)))

(defclass switch ()
  ((bit :initarg :bit :initform 0 :accessor sw-bit)
   (pos-nibble :initarg :pos-nibble :initform 0 :accessor sw-pos-nibble)
   (pos-bit :initarg :pos-bit :initform 0 :accessor sw-pos-bit)
   (x :initarg :x :initform 0 :accessor sw-x)
   (y :initarg :y :initform 0 :accessor sw-y)
   (width :initarg :width :initform 0 :accessor sw-width)
   (height :initarg :height :initform 0 :accessor sw-height)))

(defparameter *ts-huge* (make-text-style :sans-serif :bold :huge))
(defparameter *ts-large* (make-text-style :sans-serif :bold :large))

(defclass graphical-view (view) ())
(defparameter +graphical-view+ (make-instance 'graphical-view))

(define-presentation-method highlight-presentation ((type switch) record stream state)
  (with-bounding-rectangle* (x0 y0 x1 y1) record
    (with-scaling (stream 11/10 11/10 (make-point (+ x0 (/ (- x1 x0) 2))
                                                  (+ y0 (/ (- y1 y0) 2))))
      (draw-rectangle* stream x0 y0 x1 y1
                       :filled nil
                       :ink +flipping-ink+))))

(define-presentation-method present (switch (type switch) stream (view graphical-view)
                                      &key &allow-other-keys)
  (draw-switch stream (sw-x switch) (sw-y switch)
               (sw-width switch) (sw-height switch)
               (sw-pos-bit switch) (sw-bit switch)))

(defun set-switches (data switches)
  (loop for nibble in (subseq data 0 6)
        for pos-nibble from 0
        do (loop with offset = (* 3 pos-nibble)
                 and nibble = (1- nibble)
                 for sw in (subseq switches offset (+ offset 3))
                 for pos-bit from 2 downto 0
                 do (setf (sw-bit sw) (ldb (byte 1 pos-bit) nibble)))))

(defun draw-nibble (frame pane nibble pos-nibble w h x y)
  (loop with txt = (format nil "— ~D —" nibble)
        with (wtxt htxt) = (multiple-value-list (clim:text-size pane txt))
        with offset = (* 3 pos-nibble)
        with switches = (subseq (type-30-switches frame) offset (+ offset 3))
        initially (with-text-style (pane *ts-large*)
                    (draw-text* pane txt (+ x (/ (- w wtxt) 2)) (+ y htxt (/ h 4))))
        for pos-bit from 2 downto 0
        for sw in switches
        do ;;(break)
           (setf (sw-width sw) w
                 (sw-x sw) x
                 (sw-y sw) y
                 (sw-height sw) h)
           ;;(break)
           (clim:present sw 'switch :stream pane)))

(define-presentation-type button () :inherit-from 'symbol)

(define-presentation-method highlight-presentation ((type button) record stream state)
  state
  (with-bounding-rectangle* (izq arr der abj) record
    (draw-circle* stream
                  (+ izq (/ (- der izq) 2))
                  (+ arr (/ (- abj arr) 2))
                  (/ (- der izq) 1.85)
                  :filled t
                  :ink +flipping-ink+)))

(defun draw-test-word (frame pane)
  (a:when-let ((display-pane (find-pane-named frame 'display-pane)))
    (with-bounding-rectangle* (x1 y1 x2 y2) pane
      (loop with w = (- x2 x1) and h = (/ (- y2 y1) 7)
            with h/4 = (/ h 4) and wnibble = (/ w 3/2)
            ;;with y = (* h 9/16)
            with mx = (* 1/4 wnibble) and my = (/ h 1.125)
            with x = mx
            and data = (type-30-data display-pane)
            initially
               (with-text-style (pane *ts-huge*)
                 (multiple-value-bind (wt ht) (clim:text-size pane "TEST WORD")
                   (draw-text* pane "TEST WORD" (/ (- w wt) 2) (- my (/ h 4) ht))))
            for i from 0
            for nibble in (if data (subseq data 0 6) '(1 1 1 1 1 1))
            for y = (+ my (* h i))
            do (draw-line* pane x (- y h/4) x (- y (/ h 8)) :line-thickness 4 :ink +white+)
               (draw-line* pane mx (- y h/4) (+ mx wnibble) (- y h/4) :line-thickness 4 :ink +white+)
               (draw-line* pane (+ mx wnibble) (- y h/4) (+ mx wnibble) (- y (/ h 8)) :line-thickness 4 :ink +white+)
               (draw-nibble frame pane (1- nibble) i wnibble h x y)
            finally
               (draw-line* pane (+ mx wnibble) (- y h/4) (+ mx wnibble) (- y (/ h 8))
                           :line-thickness 4 :ink +white+)
               (with-output-as-presentation (pane :start 'button)
                 (draw-circle* pane (/ w 2) (- y2 y1 (/ wnibble 3)) (/ wnibble 5)

                               :fill t :ink +gray75+))))))

(define-application-frame type-30-display ()
  ((switches :initarg :switches
             :initform (loop for n from 0 below 18
                             collect (make-instance 'switch
                                                    :pos-nibble (floor n 3)
                                                    :pos-bit (- 2 (mod n 3))))
             :accessor type-30-switches)
   (animation-thread :accessor type-30-animation-thread)
   (lock :initform (bt:make-lock "Frame lock") :accessor type-30-lock))
  (:panes (display-pane (make-pane 'display-pane
                                   :background +black+
                                   :foreground +cyan+
                                   ;;:display-function 'draw
                                   :display-time nil))
          (test-word-pane (make-clim-stream-pane :background (make-rgb-color (/ #x5a 255)
                                                                             (/ #x6e 255)
                                                                             (/ #x89 255))
                                                 :foreground +white+
                                                 :default-view +graphical-view+
                                                 :scroll-bars nil
                                                 :display-function 'draw-test-word
                                                 :display-time :command-loop))
          (interactor :interactor :height 163 ; 130 105 95
                                  :scroll-bars t))
  (:layouts (default
             (horizontally (:width (* 10/8 *width*)
                            :max-width (* 10/8 *width*)
                            :height (* 9/8 *height*)
                            :max-height (* 9/8 *height*))
               (7/8 (vertically ()
                        (15/16 display-pane)
                        (1/16 interactor)))
               (1/8 test-word-pane))))
  (:menu-bar t))

(defmethod run-frame-top-level :before (frame &key &allow-other-keys)
  (let* ((pane (find-pane-named frame 'display-pane))
         (width (bounding-rectangle-width (sheet-region pane)))
         (height (bounding-rectangle-height (sheet-region pane)))
         (pixmap-0 (allocate-pixmap pane width height))
         (pixmap-1 (allocate-pixmap pane width height))
         (medium-0 (make-medium (port pane) pane))
         (medium-1 (make-medium (port pane) pane)))
    (setf (type-30-pixmap-0 pane) pixmap-0
          (type-30-pixmap-1 pane) pixmap-1
          (type-30-medium-0 pane) medium-0
          (type-30-medium-1 pane) medium-1
          (medium-drawable medium-0) pixmap-0
          (medium-drawable medium-1) pixmap-1
          (type-30-frames pane) (list)
          (type-30-animation-thread frame) (bt:make-thread (animate *application-frame*)
                                                           :name "Animation"))
    (draw-rectangle* (type-30-medium-0 pane) 0 0
                     (bounding-rectangle-width (sheet-region pane))
                     (bounding-rectangle-height (sheet-region pane))
                     :filled t
                     :ink +black+)
    (trivial-garbage:finalize
     pane (lambda ()
            (when *application-frame*
              (format *debug-io* "finalizer!!~%")
              (alexandria:when-let ((pane (find-pane-named *application-frame*
                                                           'display-pane)))
                (format *debug-io* "pane: ~S~%" pane)
                (clim:deallocate-pixmap (type-30-pixmap-0 pane))
                (clim:deallocate-pixmap (type-30-pixmap-1 pane))))))))

(defmethod handle-event ((gadget display-pane) (evento key-press-event))
  (when *application-frame*
    (with-slots (escenario teclas) *application-frame*
      (let ((tecla (keyboard-event-key-name evento)))
        (case tecla
          ((|:Q| :|q|) (when (type-30-playing gadget)
                         (execute-frame-command *application-frame* `(com-exit))))
          ((:|M| :|m|) (execute-frame-command *application-frame* `(com-minskytron :start))))))))

(define-type-30-display-command (com-minskytron :name "Minskytron" :menu nil) ((button 'button))
  (declare (ignore button))
  (format *debug-io* "Minskytron!~%")
  (let ((display (find-pane-named *application-frame* 'display-pane)))
    (bt:with-lock-held ((type-30-lock *application-frame*))
      (setf (type-30-animation-func display) #'minskytron
            (type-30-frame-number display) 0
            (type-30-frames display) (list)
            (type-30-playing display) t)
      (setf (type-30-data display)
            (if (type-30-data display)
                (gen-minskytron-pars (type-30-data display))
                (gen-minskytron-pars)))
      (set-switches (type-30-data display) (type-30-switches *application-frame*)))))

(define-type-30-display-command (com-change-switch :name "Change switch") ((switch 'switch))
  (declare (optimize (debug 3)))
  (a:when-let* ((display (find-pane-named *application-frame* 'display-pane))
                (data (type-30-data display)))
    (let ((new-bit (if (zerop (sw-bit switch)) 1 0)))
      (setf (sw-bit switch) new-bit)
      (bt:with-lock-held ((type-30-lock *application-frame*))
        (setf (elt (type-30-data display) (sw-pos-nibble switch))
              (1+ (dpb new-bit (byte 1 (sw-pos-bit switch))
                       (1- (elt (type-30-data display) (sw-pos-nibble switch))))))))))

(define-type-30-display-command (com-exit :name "Exit" :menu t) ()
  (let ((display (find-pane-named *application-frame* 'display-pane)))
    (bt:with-lock-held ((type-30-lock *application-frame*))
      (setf (type-30-stop display) t
            (type-30-playing display) nil
            (type-30-animation-func display) nil))
    (frame-exit *application-frame*)))

(define-type-30-display-command (com-pause :name "Pause" :menu t) ()
  (let ((display (find-pane-named *application-frame* 'display-pane)))
    (bt:with-lock-held ((type-30-lock *application-frame*))
      (when (type-30-playing display)
        (setf (type-30-animation-func display)
              (if (null (type-30-animation-func display))
                  #'minskytron
                  nil))))))

(define-presentation-to-command-translator change-sw-bit
    (switch com-change-switch type-30-display
      :gesture :select
      :documentation "Change switch"
      :echo t)
    (object)
  (list object))

(define-presentation-to-command-translator button-click
    (button com-minskytron type-30-display
      :gesture :select
      :documentation "Start minskytron"
      :echo t)
    (object)
  (list object))

(defun type-30-display-entry-point ()
  (apply 'main *command-line-arguments*))
