(in-package :om)

;========================= Multithreading with lispwork ====================

(defun loop-until-finish-process (mailbox)
      (loop :with mailbox-empty = nil 
            :do 
                (if (not (equal mailbox-empty (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) mailbox)))) 
                    (let* (
                        (number_of_thread (abs (- (length mailbox) (length (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) mailbox)))))))
                        (if (not (equal 0 number_of_thread))
                            ;; print "Thread ~d Finalized!%" + new line 
                            (format t "Thread ~d Finalized!~%" number_of_thread))))
            :while (setf mailbox-empty (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) mailbox))) 
            :finally (return mailbox-empty)))

;========================= 

(defun ckn-mailbox-name (list-of-something)

(let* ()
      (loop :for chunks-number :in (arithm-ser 1 (length list-of-something) 1)
            :collect (ompd-list->string-fun (list 'mailbox- (om::om-random 100 999) chunks-number)))))

;========================= 

(defun ckn-mailbox-peek (mail-box)
(mapcar (lambda (x) (mp:mailbox-peek x)) mail-box))

;================================================

(defun ckn-make-mail-box (names-of-all-process)
(loop :for name-process :in names-of-all-process
      :collect (mp:make-mailbox :lock-name name-process)))

;; ====================================================

(defun loop-in-parts (sound-bytes-window window hop-size &optional result)

(let* (
      (action1 (first-n sound-bytes-window window))
      (action2 (let* ((number (- (length sound-bytes-window) hop-size)))
                      (if (plusp number)      
                        (last-n sound-bytes-window number)
                        sound-bytes-window))))
(if (or (< (length (remove nil action2)) window) (equal action1 action2))
    (if (equal action1 action2) 
        (reverse (om::x-append (list action1) result))
        (reverse (om::x-append (list action2) (list action1) result)))
  (setf sound-bytes-window (loop-in-parts action2 window hop-size (push action1 result))))))

;; ====================================================

(defmethod! ckn-loop-multi-prepare ((list list) (how_many_threading list))
:initvals ' (nil nil)       
:indoc ' ("one list" "how much threading for time.")
:outdoc ' ("result")
:icon 'multithreading 
:doc "It does multithreading loops, do not use it if you REALLY do not need :) ."


(loop-in-parts list how_many_threading how_many_threading))

;; =============================================

(defmethod! ckn-multithreading-prepare ((list list) (how_many_threading number))
:initvals ' (nil nil)       
:indoc ' ("one list" "how much threading for time.")
:outdoc ' ("result")
:icon 'multithreading 
:doc "It does multithreading loops, do not use it if you REALLY do not need :) ."


(loop-in-parts list how_many_threading how_many_threading))

;; ====================================================

(defmethod! ckn-multi-1-var ((ckn-lambda function) (list list) &optional (loop-inside 0))
:initvals ' (nil nil)       
:indoc ' ("one function" "one list" "loop inside function?")
:menuins '((2 (("yes" 1) ("no" 0))))
:outdoc ' ("result")
:icon 'multithreading 
:doc "It does multithreading loops, do not use it if you REALLY do not need :) ."

(let* (
      (list-of-something (if (equal loop-inside 0) list (mapcar (lambda (x) (list x)) list)))
      (action1 (ckn-mailbox-name list-of-something))
      (action2 (ckn-make-mail-box action1)))
(loop 
    :for list-of-something-loop :in list-of-something 
    :for create-mailbox :in action2
    :for names-process :in action1
    :for index :from 1 :to (length list) 
    :do (om::om-print (format nil "Thread ~d of ~d" index (length list)) "OM-CKN ::")  
    :do 
        (mp:process-run-function names-process
                 () 
                  (lambda (x y) (mp:mailbox-send x (mapcar ckn-lambda (om::list! y)))) create-mailbox (list list-of-something-loop)))
(loop-until-finish-process action2)
(ckn-mailbox-peek action2)))


;; ==================================================