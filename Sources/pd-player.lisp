(in-package :om)

;; ============================================= MICROTONAL PLAYER WITH PUREDATA ==========================

(setf *PureData-PLAY-STATE* nil)

;; =============================================================================================================

(add-preference-section :externals "PureData" nil '(:PureData :Pd-Patches))
#-linux(add-preference :externals :PureData "Pure Data executable" :file " ")
#+linux(add-preference :externals :PureData "Pure Data executable" :string "pd ")
(add-preference :externals :Pd-Patches "Pure Data Patches" :folder (merge-pathnames "Pd-Patches/" (lib-resources-folder (find-library "OM-pd"))))


;; add some preferences :SoundFont
(add-preference-section :externals "PD OSC player" nil '(:SoundFont-Folder :SoundFont :PureData-Player)) 
(add-preference :externals :PureData-Player "PureData Player" :bool t "If checked, the PureData player will be used to play the score.")
(add-preference :externals :SoundFont-Folder "SoundFound Folder" :folder "Choose a SoundFound Folder")

;; =============================================================================================================

;;; The piece of Code will define the *all-available-soundfonts* to be choosed in the preference 

(let* (
      (thepath (get-pref-value :externals :SoundFont-Folder))
      (thefilelist-sf2 (om::om-directory thepath  ; "*.sf2"
                                    :type "sf2" 
                                    :directories t 
                                    :files t 
                                    :resolve-aliases nil 
                                    :hidden-files nil))
      
      (thefilelist-sf3 (om::om-directory thepath ; "*.sf3"
                                    :type "sf3"
                                    :directories t 
                                    :files t 
                                    :resolve-aliases nil 
                                    :hidden-files nil))
      
      (thefilelist (append thefilelist-sf2 thefilelist-sf3)) ;; append sf2 and sf3
      
      (check_files_inside_folder 
                  (loop :for loop-files :in thefilelist 
                        :collect (if 
                                    (system::directory-pathname-p loop-files)
                                    (ompd-search-inside-some-folder loop-files extension)
                                    loop-files)))) ;; Recursive: check sf2 and sf3 inside folders 

      (setf *all-available-soundfonts*  (mapcar (lambda (sf2&sf3) (ompd-get-filename sf2&sf3)) (remove nil (om::flat check_files_inside_folder)))))

(add-preference :externals :SoundFont "SoundFound" *all-available-soundfonts*  (car *all-available-soundfonts* )) ;; add-preference in OM-Sharp Menu.

;; ========================================================================


(defun pd-player-open-PD ()

(mp:process-run-function "Open PD"
                 () 
                  (lambda ()
                          (progn 
                                (mp:process-run-function "Run PD in Backgroud!"
                                            () 
                                              (lambda () (pd~ 
                                                            (pd-define-patch "Microtonal-player.pd") ;; Here is the PD patch
                                                                :var (list (om::x-append 'soundfont (probe-file (om::string+ (om::string+ (get-pref-value :externals :SoundFont-Folder) (get-pref-value :externals :SoundFont))))))
                                                                :gui nil
                                                                :offline nil 
                                                                :sound-out (tmpfile "PD.wav") ;; This will not be used just to not need to redefine the pd~ function.
                                                                :verbose nil)))))))


;; =============================================


(defun close-UDP-server ()
(loop :for udp-server :in *running-udp-servers*
                  :do (if (equal (mp:process-name (third udp-server)) "UDP receive server on \"127.0.0.1\" 3320")
                  (progn (om::om-stop-udp-server (third udp-server))))))


;; ========================================================================

(defun pd-player-wait-pd ()

       (progn 
              (setf *pd-is-open* nil)

              (om-start-udp-server 3320 "127.0.0.1" (lambda (msg) (let () (if  (equal (car (cdr (osc-decode msg))) 1.0)
                                                                              (progn (setf *pd-is-open* t) nil)
                                                                              )))) ;;; When PD is open, the loadbang will sent one 1 to this port.

              (loop :with pd-start = nil 
                  :while (null *pd-is-open*)
                  :finally (om::om-print "PD is open!" "OM-pd")) ;; Wait PD to be open!

              (close-UDP-server)
                  
              (sleep 1) ;; better
                  )) ;; Remove the UDP server to check is the PD is open




; ====================================================================================================
; ====================================================================================================
; ================================= OM-KEYS ==========================================================
; ================================= OM-ALTERATION IN HOW OM-SHARP WORKS ==============================
; ====================================================================================================

(defmethod object-default-edition-params ((self score-element))
  '((:font-size 24)
    (:staff :g)
    (:scale :scale-1/2)
    (:duration-display nil)
    (:velocity-display :hidden)
    (:channel-display :hidden)
    (:midiport-display nil)
    (:h-stretch 1)
    (:groups nil)
    (:group-names t)
    (:selected-group :all)
    (:player :osc)))

;; ===================================================================================================

(defmethod editor-key-action ((editor patch-editor) key)
  (declare (special *general-player*))

  (let* ((panel (get-editor-view-for-action editor))
         (selected-boxes (get-selected-boxes editor))
         (selected-connections (get-selected-connections editor))
         (player-active (and (boundp '*general-player*) *general-player*)))

    (when panel

      (if (and (om-action-key-down) selected-boxes)
          ;; works with special keys (arrows, enter, ...)
          ;; 'normal' keys are caught through the menu shortcut system
          (loop for box in selected-boxes do (box-key-action box key))

        (case key

              (#\Space (progn        
                            ; TODO: Generate a aleatoric number to, when the player is active, and stop, 
                            (if *PureData-PLAY-STATE*
                                   (progn
                                          (if (get-pref-value :externals :PureData-Player) (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 3000)) ;; kill PD if it is running (Just one process simmultaneously)
                                          (setf *PureData-PLAY-STATE* nil)
                                          (when player-active (play/stop-boxes selected-boxes)))
                                   (progn
                                          
                                          
                                          (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 3000) ;; before start, kill PD if it is running (Just one process simmultaneously)
                                          (if (get-pref-value :externals :PureData-Player) (pd-player-open-PD))
                                          (setf *PureData-PLAY-STATE* t)
                                          (if (get-pref-value :externals :PureData-Player)  (pd-player-wait-pd)) ;; Wait until PD is open
                                          (when player-active (play/stop-boxes selected-boxes))))
                                          (let*  (
                                                 (play-boxes (remove-if-not 'play-box? selected-boxes)))
                                                 (if (get-pref-value :externals :PureData-Player) 
                                                        (mp:process-run-function "Run PD in Backgroud!"
                                                               () 
                                                                      (lambda ()
                                                                             (let* (
                                                                                    
                                                                                    ;(find-player (print (player  (car play-boxes))))
                                                                                    (objects (mapcar 'get-obj-to-play play-boxes))
                                                                                    (dur-of-objects (mapcar 'object-dur objects))
                                                                                    (max-dur (ms->sec (+ (list-max dur-of-objects) 60))))
                                                                                    (mapcar 'player-info objects)
                                                                                    (sleep max-dur)
                                                                                    (setf *PureData-PLAY-STATE* nil)
                                                                                    ;(om-print "Closing PD!" "OM-pd")
                                                                                    (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 3000))))))))
                                          

                     
              (#\s (when player-active (stop-boxes selected-boxes)))

              (:om-key-delete (unless (edit-lock editor)
                                   (store-current-state-for-undo editor)
                                   (remove-selection editor)))

              (#\n (if selected-boxes
                     (mapc 'set-show-name selected-boxes)
                     (unless (edit-lock editor)
                     (make-new-box panel))))

              (#\p (unless (edit-lock editor)
                     (make-new-abstraction-box panel)))

              (:om-key-left (unless (edit-lock editor)
                            (if (om-option-key-p)
                                   (when selected-boxes
                                   (store-current-state-for-undo editor)
                                   (mapc 'optional-input-- selected-boxes))
                                   (let ((selection (or selected-boxes selected-connections)))
                                   (store-current-state-for-undo editor :action :move :item selection)
                                   (mapc
                                   #'(lambda (f) (move-box f (if (om-shift-key-p) -10 -1) 0))
                                   selection))
                                   )))
              (:om-key-right (unless (edit-lock editor)
                            (if (om-option-key-p)
                                   (when selected-boxes
                                   (store-current-state-for-undo editor)
                                   (mapc 'optional-input++ selected-boxes))
                                   (let ((selection (or selected-boxes selected-connections)))
                                   (store-current-state-for-undo editor :action :move :item selection)
                                   (mapc #'(lambda (f) (move-box f (if (om-shift-key-p) 10 1) 0))
                                          selection))
                                   )))
              (:om-key-up (unless (edit-lock editor)
                            (store-current-state-for-undo editor :action :move :item (or selected-boxes selected-connections))
                            (mapc #'(lambda (f) (move-box f 0 (if (om-shift-key-p) -10 -1)))
                                   (or selected-boxes selected-connections))
                            ))
              (:om-key-down (unless (edit-lock editor)
                            (store-current-state-for-undo editor :action :move :item (or selected-boxes selected-connections))
                            (mapc #'(lambda (f) (move-box f 0 (if (om-shift-key-p) 10 1)))
                                   (or selected-boxes selected-connections))
                            ))

              (#\k (unless (edit-lock editor)
                     (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'keyword-input++ selected-boxes))))
              (#\+ (unless (edit-lock editor)
                     (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'keyword-input++ selected-boxes))))
              (#\K (unless (edit-lock editor)
                     (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'keyword-input-- selected-boxes))))
              (#\- (unless (edit-lock editor)
                     (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'keyword-input-- selected-boxes))))

              (#\> (unless (edit-lock editor)
                     (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'optional-input++ selected-boxes))))
              (#\< (unless (edit-lock editor)
                     (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'optional-input-- selected-boxes))))

              (#\b (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'switch-lock-mode selected-boxes)))

              (#\1 (unless (or (edit-lock editor) (get-pref-value :general :auto-ev-once-mode))
                     (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'switch-evonce-mode selected-boxes))))

              (#\l (unless (edit-lock editor)
                     (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'switch-lambda-mode selected-boxes))))

              (#\m (mapc 'change-display selected-boxes))


              ;;; Box editing
              ;;; => menu commands ?

              (#\A (unless (edit-lock editor)
                     (store-current-state-for-undo editor)
                     (align-selected-boxes editor)))

              (#\S (unless (edit-lock editor)
                     (store-current-state-for-undo editor)
                     (let ((selection (append selected-boxes selected-connections)))
                     (mapc 'consolidate-appearance selection)
                     (update-inspector-for-editor editor nil t))))

              (#\c (unless (edit-lock editor)
                     (store-current-state-for-undo editor)
                     (if selected-boxes
                            (auto-connect-box selected-boxes editor panel)
                     (make-new-comment panel))))

              (#\C (unless (edit-lock editor)
                     (store-current-state-for-undo editor)
                     (auto-connect-seq selected-boxes editor panel)))

              (#\r (unless (edit-lock editor)
                     (store-current-state-for-undo editor)
                     (mapc 'set-reactive-mode (or selected-boxes selected-connections))))

              (#\i (unless (edit-lock editor)
                     (store-current-state-for-undo editor)
                     (mapc 'initialize-size (or selected-boxes selected-connections))))

              (#\I (mapc 'initialize-box-value selected-boxes))

              (#\r (unless (edit-lock editor)
                     (store-current-state-for-undo editor)
                     (mapc 'set-reactive-mode (or selected-boxes selected-connections))))

              ;;; abstractions
              (#\a (unless (edit-lock editor)
                     (when selected-boxes
                     (store-current-state-for-undo editor)
                     (mapc 'internalize-abstraction selected-boxes))))

              (#\E (unless (edit-lock editor)
                     (encapsulate-patchboxes editor panel selected-boxes)))

              (#\U (unless (edit-lock editor)
                     (unencapsulate-patchboxes editor panel selected-boxes)))

              (#\L (unless (edit-lock editor)
                     (store-current-state-for-undo editor)
                     (list-boxes editor panel selected-boxes)))

              (#\v (eval-editor-boxes editor selected-boxes))

              (#\w (om-debug))

              (#\h (funcall (help-command editor)))

              (#\d (when selected-boxes
                     (mapcar #'print-help-for-box selected-boxes)))


              ; ======================================== 
              ; ========== OM-PY =======================
              ; ========================================

              (#\z (if   (and  selected-boxes 
                        (find-library "OM-py") 
                        (or 
                              (equal (type-of (car (om::list! selected-boxes))) 'omboxpy) 
                              (equal (type-of (car (om::list! selected-boxes))) 'OMBox-run-py)))

              ; if omboxpy or OMBox-run-py is selected, then open the VS-code

                  (progn
                     (om::om-print "Opening VScode!" "OM-Py")
                     (defparameter *vscode-opened* t)
                     (open-vscode selected-boxes))

              ; if nothing is selected, then new py-script

                  (unless (edit-lock editor)
                            ;(print panel)
                            (make-new-py-box panel))))
        
       ; ======================================== 
       ; ========== OM-PY =======================
       ; ======================================== 
              (otherwise nil))
                                   ))))



;;; 

(defmethod editor-key-action :around ((self play-editor-mixin) key)
  (case key
    (#\Space (progn        
              ; TODO: Generate a aleatoric number to 
              ; TODO: Make this a function of the editor's size
              (if *PureData-PLAY-STATE*
                     (progn
                            (if (get-pref-value :externals :PureData-Player) (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 3000)) ;; kill PD if it is running (Just one process simmultaneously)
                            (setf *PureData-PLAY-STATE* nil)
                            (when (and (boundp '*general-player*) *general-player*) (editor-play/pause self)))
                     (progn
                            
                            
                            (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 3000) ;; before start, kill PD if it is running (Just one process simmultaneously)
                            (if (get-pref-value :externals :PureData-Player) (pd-player-open-PD))
                            (setf *PureData-PLAY-STATE* t)
                            (if (get-pref-value :externals :PureData-Player)  (pd-player-wait-pd)) ;; Wait until PD is open
                            (when (and (boundp '*general-player*) *general-player*) (editor-play/pause self))))
                                   (if (get-pref-value :externals :PureData-Player) 
                                          (mp:process-run-function "Run PD in Backgroud!"
                                                 () 
                                                        (lambda ()
                                                               (let* (
                                                                      
                                                                      (dur-of-objects (object-dur (get-obj-to-play self)))
                                                                      (max-dur (ms->sec (+ (list-max dur-of-objects) 60))))
                                                                      (sleep max-dur)
                                                                      (setf *PureData-PLAY-STATE* nil)
                                                                      ;(om-print "Closing PD!" "OM-pd")
                                                                      (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 3000)))))))
    
    (#\p (editor-play/pause self) t)
    (#\s (editor-stop self) t)
    (:om-key-esc
     (when (eq (player-get-object-state (player self) (get-obj-to-play self)) :stop)
       (if (equal '(0 0) (play-interval self))
           (call-next-method) ;; if the interval is already reset: check if there is another 'escape' to do
         (editor-reset-interval self)))
     (editor-stop self)
     (call-next-method)
     t)
    (otherwise (call-next-method))
    ))