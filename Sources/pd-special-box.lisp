(in-package :om)



;;;===============================================================
;;; PATCH SELECTION, FILE BY JEAN BRESSON
;;;===============================================================

;
(defclass PatchSelectionBox (OMInterfaceBox)
  ((items :accessor items :initarg :items :initform (pd-patches-list))
   (selection :accessor selection :initarg :selection :initform nil)
   (multiple-selection :accessor multiple-selection :initarg :multiple-selection :initform nil)
   (cell-height :accessor cell-height :initarg :cell-height :initform 12)
   (cell-font :accessor cell-font :initarg :cell-font :initform (om-def-font :normal))
   (output-mode :accessor output-mode :initarg :output-mode :initform :value))
  (:documentation "An interface box to graphically select among different items in a list.
Use the optional inputs to set the list of items.
Click with CMD (Mac) or Shift+Ctrl (Windows/Linux) to change the selected item.
Returns the selected item, or the selected index depending on how this is set in the box properties.
Can return a list of selected items if 'multiple selection' is enabled in the box properties."
   ))


(defmethod special-box-p ((self (eql 'pd-list-patches))) t)
(defmethod special-item-reference-class ((item (eql 'pd-list-patches))) 'PatchSelectionBox)

(defmethod default-size ((self PatchSelectionBox)) (omp 180 (* (length (items self)) (cell-height self))))
(defmethod maximum-size ((self PatchSelectionBox))
  (omp nil (max 60 (+ 16 (* (+ 2 (cell-height self)) (length (items self)))))))

(defmethod get-all-keywords ((self PatchSelectionBox))
  '((:items)))


(defmethod get-properties-list ((self PatchSelectionBox))
  (add-properties (call-next-method)
                  "List selection display"
                  `((:multiple-selection "Multiple selection" :bool multiple-selection)
                    (:cell-height "Cell size (px)" :number cell-height (2 40))
                    (:cell-font "Cell font" :font cell-font)
                    (:output-mode "Output mode" (:value :index) output-mode-accessor)
                    )))


(defmethod update-value-from-selection ((self PatchSelectionBox))
  (set-value self
             (if (multiple-selection self)

                 (if (equal (output-mode self) :value)
                     (list (posn-match (items self) (selection self)))
                   (list (selection self)))

               (if (equal (output-mode self) :value)
                   (and (selection self)
                        (list (nth (car (selection self)) (items self))))
                 (list (car (selection self)))
                 )
               )))

(defmethod output-mode-accessor ((self PatchSelectionBox) &optional value)
  (when value
    (setf (output-mode self) value)
    (update-value-from-selection self))
  (output-mode self))

(defmethod apply-box-attributes ((self PatchSelectionBox) attributes)
  (if (null (value self))
      (progn (om::om-message-dialog "There is no Patch selected!") (om::abort-eval)))
  
  
  (when attributes
    (let ((newlist (getf attributes :items)))
      (unless (equal newlist (items self))
        (setf (selection self) nil)
        (set-value self nil))

      (let ((min-size (+ 8 (* (+ 2 (cell-height self)) (length newlist)))))
        (when (< (box-h self) min-size)
          (omng-resize self (omp (box-w self) min-size))
          (reset-frame-size (frame self)))
        )
      ))
  (call-next-method))


(defmethod omng-save ((self PatchSelectionBox))
  (append (call-next-method)
          `((:items ,(omng-save (items self)))
            (:selection ,(omng-save (selection self))))))

(defmethod load-box-attributes ((box PatchSelectionBox) data)
  (setf (items box) (omng-load (find-value-in-kv-list data :items)))
  (setf (selection box) (omng-load (find-value-in-kv-list data :selection)))
  box)


(defmethod omNG-make-special-box ((reference (eql 'pd-list-patches)) pos &optional init-args)
  (let* ((box (make-instance 'PatchSelectionBox
                             :name "pd-list-patches"
                             :reference 'pd-list-patches)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))

(defmethod draw-interface-component ((self PatchSelectionBox) x y w h)
  (om-with-clip-rect (frame self) x y w h

    (let* ((text-h (cadr (multiple-value-list (om-string-size "A" (cell-font self)))))
           (text-pos (if (>= text-h (cell-height self)) (cell-height self) (* .5 (+ (cell-height self) text-h)))))

      (om-with-font
       (cell-font self)
       (loop for i = 0 then (+ i 1)
             for yy = y then (+ yy (cell-height self))
             while (< yy h)
             while (< i (length (items self)))
             do
             (when (member i (selection self))
               (om-draw-rect 3 (+ yy 2) (- w 6) (cell-height self) :fill t :color (om-def-color :dark-gray)))
             (om-draw-string 5 (+ yy text-pos) (format nil "~A" (nth i (items self)))
                             :color (if (member i (selection self)) (om-def-color :white) (om-def-color :black)))
             ))
      )

    (when (> (* (cell-height self) (length (items self))) h)
      (om-draw-rect (- w 20) (- h 8) 16 10 :fill t :color (om-def-color :white))
      (om-draw-string (- w 18) (- h 2) "...")
      )
    ))


(defmethod interfacebox-action ((self PatchSelectionBox) frame pos)

  (when (or (om-action-key-down)
            (container-frames-locked (om-view-container frame)))

    (let* ((y (- (om-point-y pos) 4))
           (n (floor y (cell-height self))))
      (when (and (> (om-point-x pos) 5)
                 (< (om-point-x pos) (- (w frame) 10))
                 (< n (length (items self))))

        (store-current-state-for-undo (editor (container self)))

        (if (member n (selection self))

            (setf (selection self) (remove n (selection self)))

          (setf (selection self)
                (if (multiple-selection self)
                    (sort (cons n (selection self)) '<)
                  (list n))))

        (update-value-from-selection self)

        (when (reactive (car (outputs self))) (self-notify self))
        (om-invalidate-view frame)
        ))))


(defmethod box-key-action ((box PatchSelectionBox) key)
  (case key
    (:om-key-up
     (when (items box)
       (let* ((ref-selection (or (car (selection box)) 0))
              (new-selection (max 0 (1- ref-selection))))
         (setf (selection box) (list new-selection))
         (update-value-from-selection box)

         (when (reactive (car (outputs box))) (self-notify box))
         (om-invalidate-view (frame box)))))

    (:om-key-down
     (when (items box)
       (let* ((ref-selection (or (car (selection box)) 0))
              (new-selection (min (1- (length (items box))) (1+ ref-selection))))
         (setf (selection box) (list new-selection))
         (update-value-from-selection box)

         (when (reactive (car (outputs box))) (self-notify box))
         (om-invalidate-view (frame box)))))

    (otherwise (call-next-method))))
