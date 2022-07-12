;;; ============================================

(defmethod puredata-player ((voice voice) caller)

(if *PureData-PLAY-STATE*
      (progn
                  (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 1996) ;; kill PD if it is running (Just one process simmultaneously)
                  (setf *PureData-PLAY-STATE* nil))

(progn
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
                                                                :verbose nil))))))

            (setf *pd-is-open* nil)

            (om-start-udp-server 3320 "127.0.0.1" (lambda (msg) (let () (if  (equal (car (cdr (osc-decode msg))) 1.0)
                                                                              (progn (setf *pd-is-open* t) nil)
                                                                              )))) ;;; When PD is open, the loadbang will sent one 1 to this port.

            (loop :with pd-start = nil 
                  :while (null *pd-is-open*)
                  :finally (om::om-print "PD is open!" "OM-pd")) ;; Wait PD to be open!

            
            (loop :for udp-server :in *running-udp-servers*
                  :do (if (equal (mp:process-name (third udp-server)) "UDP receive server on \"127.0.0.1\" 3320")
                  (progn (om::om-stop-udp-server (third udp-server))))) ;; Remove the UDP server to check is
                                                                          ;; the PD is open!
            
            
            ; This let will send the score data to PureData that will play it!
            ; For now I am using the sleep function to wait and put it on time. There are some better way??
            (let* (
                  (score-lonset (lonset voice)) ;; All onsets of notes
                  (dx-lonset (om::x-append (car score-lonset) (om::x->dx score-lonset))) ;; if the first figure is a Rest.
                  (score-data (mat-trans (list (om::lmidic voice) (om::lvel voice) (om::lchan voice) (om::ldur voice)))) ;; organize the data by (note vel chan dur)
                  (last-duration (car (om::list! (om::ms->sec (car (last (om::ldur voice))))))))
                  (setf *PureData-PLAY-STATE* t) ; check if there are other voices playing
                  (loop :for onsets :in dx-lonset ; start the loop
                        :for notes :in score-data 
                        :while *PureData-PLAY-STATE*
                        :do (let* (
                                    (the-notes (list notes))) ; the notes to be played
                                    (sleep (om::ms->sec onsets)) ; wait the time of the onset
                                    (mapcar (lambda (x) ;; send data already in time to PureData
                                                (mapcar 
                                                      (lambda (notes lvel lchan ldur) 
                                                            (let* (
                                                                  (data2send (om::x-append notes lvel lchan ldur)) ;; last format of data
                                                                  (format-msg (om::osc-msg "/note" data2send))) ;; create OSC message
                                                                  (om::osc-send format-msg "127.0.0.1" 1996)))    (first x) ;; note
                                                                                                                  (second x) ;; vel
                                                                                                                  (third x) ; lchannel 
                                                                                                                  (fourth x))) ; duration
                                                                                                                              the-notes))) ;; send data to PureData
                              
                        (sleep last-duration) ;; Wait last duration of the voice/chord

                        ;(box-player-stop caller) ;; How to make the timeline (I don't know the name of it) || red bar in the score
                        (when (and (boundp '*general-player*) *general-player*) (play/stop-boxes *OM-Pd_PureData-object-playing*)) ;; Stop the boxes
                        (if *PureData-PLAY-STATE* (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 1996)) ;; tell PD that the process if finished and kill it
                        (if *PureData-PLAY-STATE* (setf *PureData-PLAY-STATE* nil)))))) ;; 
      

;; =====================================================================
;; Redefine the method that will play the score

(defmethod player-stop-object ((self scheduler) (object score-element))
  
  ;; This is OM-Sharp CODE 

  (send-current-midi-key-offs object)
  (when (and (equal :auto-bend (get-pref-value :score :microtone-bend))
             *micro-channel-mode-on*)
    (loop for p in (collec-ports-from-object object) do (micro-reset p)))

  ;; Finish of OM-Sharp CODE 
  
  (if (get-pref-value :externals :PureData-Player)
      (progn 
              ;(om::om-print "Closing PD" "OM-CKN")
              
              (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 1996)))
   
  (call-next-method)) ;; This is OM-Sharp CODE 

;; =====================================================================

(defmethod PD-player-play-object ((self scheduler) (object score-element) (caller ScoreBoxEditCall) &key parent interval)
  
  (declare (ignore parent interval)) ;; This is OM-Sharp CODE 
  
      (mp:process-run-function "Open PD" () (lambda () (puredata-player object caller)))) ;; Open Function to play in PureData
      ;(box-player-start object))

;;; =======================

;; This is the method used when SPACE in pressed!!

(defmethod play/stop-boxes ((boxlist list))
  (let* ((play-boxes (remove-if-not 'play-box? boxlist)))
      
      (if (find-if 'play-state play-boxes)
        
        ;;; stop all
        (mapc #'(lambda (box) (player-stop-object *general-player* (get-obj-to-play box)) (box-player-stop box)) play-boxes)
      
        ;;; start all
        (if (get-pref-value :externals :PureData-Player)
            
            (if (member (type-of (car boxlist)) (list 'scoreboxeditcall 'voice 'chord 'chord-seq 'note))
                  (mapc (lambda (box) (when (play-obj? (get-obj-to-play box)) (PD-player-play-object *general-player* (get-obj-to-play box) box) (box-player-start box))) play-boxes)
                  (mapc (lambda (box) (when (play-obj? (get-obj-to-play box)) (player-play-object *general-player* (get-obj-to-play box) box) (box-player-start box))) play-boxes))

            (mapc 
                  (lambda (box) 
                      (when (play-obj? (get-obj-to-play box)) (player-play-object *general-player* (get-obj-to-play box) box) (box-player-start box)))
            play-boxes)))))




