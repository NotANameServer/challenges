(defconstant +guess-min+ 1)
(defconstant +guess-max+ 1000000)

(setf *random-state* (make-random-state t))
(defvar *to-guess* (+ +guess-min+ (random +guess-max+)))

(defun is-guess-correct (guess)
  (cond
    ((< guess *to-guess*) 1)
    ((> guess *to-guess*) -1)
    (T 0)))

(defun guess (lr hr counter)
  (if (> counter 50)
    (progn
      (format T "Failed to guess number. Was ~a.~%" *to-guess*)
      NIL)
    (let* ((guess (+ lr (floor (/ (- hr lr) 2))))
           (check (is-guess-correct guess)))
      (format T "(~a;~a) -> ~a~%" lr hr guess)
      (cond
        ((= check 0) counter)
        ((= check -1) 
         (format T "Less.~%")
         (guess lr guess (1+ counter)))
        ((= check 1)
         (format T "More.~%")
         (guess guess hr (1+ counter)))))))


(let ((guessed (guess +guess-min+ +guess-max+ 1)))
  (when guessed
    (format T "Number found in ~a tries." guessed)))
