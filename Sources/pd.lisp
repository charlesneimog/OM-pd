(in-package :om)

(setf new-thread nil)

; ============================================================================================

(defun path2wsl (path)
"Converts a Windows path to a WSL path!"
  (om::string+ "/mnt/c" (replace-all (replace-all (namestring path) "\\" "/") "C:" "")))

; ========================================== FUNCTIONS ==========================================

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement. From http://cl-cookbook.sourceforge.net/strings.html"
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


; ================================================================

(defclass! pure-data ()
    (
        #+Windows(pd :initform (ompd-list->string-fun (list (namestring (get-pref-value :externals :PureData)))) :initarg :pd :accessor pd)
        #-Windows(pd :initform (namestring (get-pref-value :externals :PureData)) :initarg :pd :accessor pd)
        (pd-path :initform nil :initarg :pd-path :accessor pd-path)
        (command-line :initform nil :initarg :command-line :accessor command-line)
        (pd-outfile :initform nil :initarg :pd-outfile :accessor pd-outfile)))

; ================================================================

(defmethod! pd-multitask-test ((patch list))
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc "It tests if a multitask is working, must be used after the pd-mk-line function."

(if (not (find-library "OM-CKN"))
    (progn  
                   (if (om-y-or-n-dialog "Pd-multithreading require the library OM-CKN, want to download it?")
                       (hqn-web:browse "https://github.com/charlesneimog/OM-CKN/releases/"))
                   (abort-eval)))
(progn (oa::om-command-line (print (om::command-line (car (om::list! patch))))) (pd-outfile (car (om::list! patch)))))
        
        


; ================================================================

(defmethod! pd-kill ()
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc "It kill ALL processes of PureData."

(if (om-y-or-n-dialog "This will kill ALL processes of PureData, are you sure?")
    #+Windows (oa::om-command-line "taskkill /IM pd.exe /F")
    #+Linux (oa::om-command-line "killall pd")
    #+Mac (oa::om-command-line "killall pd")
    ))

; ================================================================
(defmethod! pd~ ((patch string) &key (sound-in nil) (sound-out nil) (var nil) (gui nil) (offline nil) (verbose nil) (thread nil))
(pd~ (pd-define-patch patch) :sound-in sound-in :sound-out sound-out :var var :gui gui :offline offline :verbose verbose :thread thread))


(defmethod! pd~ ((patch pathname) &key (sound-in nil) (sound-out nil) (var nil) (gui nil) (offline nil) (verbose nil) (thread nil))
(pd~ (pd-define-patch patch) :sound-in sound-in :sound-out sound-out :var var :gui gui :offline offline :verbose verbose :thread thread))

; ================================================================
(defmethod! pd~ ((patch pure-data) &key (sound-in nil) (sound-out nil) (var nil) (gui nil) (offline nil) (verbose nil) (thread nil))
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc "This object is responsible for formatting a command line that will start and run PureData, what it always returns is the sound-out."


(let* (
      (sound-in (case (type-of sound-in)
                      (sound (if (null (om::file-pathname sound-in))
                                 (car (om::list! (save-temp-sounds sound-in (om::string+ "format-" (format nil "~7,'0D" (om-random 0 999999)) "-"))))
                                 (car (om::list! (om::file-pathname sound-in)))))
                      (pathname sound-in)
                      (string (probe-file sound-in))
                      (lispworks:simple-text-string (probe-file sound-in))
                      (null nil)

                      ))
      (sound-out (case (type-of sound-out)
                      (sound (if (null (om::file-pathname sound-out))
                                 (car (om::list! (save-temp-sounds sound-out (om::string+ "format-" (format nil "~7,'0D" (om-random 0 999999)) "-"))))
                                 (car (om::list! (om::file-pathname sound-out)))))
                      (pathname sound-out)
                      (string (probe-file sound-out))
                      (lispworks:simple-text-string (probe-file sound-out))
                      (null (om::tmpfile "sound.wav")))))


(if thread
    (mp:process-run-function "Open PureData"
                 () 
                  (lambda () (ckn-pd~ sound-in sound-out patch var gui offline verbose)))
    (ckn-pd~ sound-in sound-out patch var gui offline verbose thread))))
 


; ============================== TO RUN PATCHES ==================

(defun ckn-pd~ (sound-in sound-out patch var gui offline verbose thread)

;; Check if outfile have some space;;;
(let* (
        (pd-outfile (om::string-to-list (namestring sound-out) " "))
        (length-of-path (length pd-outfile)))
    (if (> length-of-path 1)
        (progn
                (om::om-message-dialog "The outfile pathname have spaces in it, it will not work")
                (om::abort-eval))
        nil))


;; Here is the real work

(let* (

    (check-if-some-var-have-spaces (loop :for all-var :in var 
                                         :for var-index :from 1 :to (length var)
                                         :collect (if (or (equal (type-of (car (cdr all-var))) 'pathname) (equal (type-of (car (cdr all-var))) 'string))
                                                    (let* (
                                                        (path (probe-file (car (cdr all-var))))
                                                        (length-of-path (length (om::string-to-list (namestring path) " "))))
                                                        (if (> length-of-path 1)
                                                            (let* (
                                                                (message (om::om-print (format nil "The pathname in the ~d spaces in it, coping to temp-files." (car all-var)) "OM-pd"))
                                                                (copy-to-tmp-files (om::tmpfile (om::string+ (write-to-string var-index) "." (car (last (om::string-to-list (namestring path) ".")))))))
                                                                (system::copy-file path copy-to-tmp-files)
                                                                (om::x-append (car all-var) copy-to-tmp-files))
                                                                (om::x-append (car all-var) path)))
                                                    all-var)))
    ;(verbose (print check-if-some-var-have-spaces))
    (outfile (replace-all (namestring sound-out) "\\" "/"))
    (tmp-infile-name (if sound-in (om::tmpfile (om::string+ (write-to-string (om::om-random 10000 99999)) "." (car (last (om::string-to-list (namestring sound-in) ".")))))))
    (int-copy-to-tmpfile (if sound-in (om::om-copy-file sound-in tmp-infile-name)))
    (fixed_infile (if sound-in (om::string+ " -send " (ompd-list->string-fun (list (om::string+ "infile " (replace-all (namestring int-copy-to-tmpfile) "\\" "/") ", "))))))
    (fixed_outfile (if sound-out (om::string+ " -send " (ompd-list->string-fun (list (om::string+ "outfile " (replace-all (namestring outfile) "\\" "/") ", "))))))
    (make_var 
            (loop :for all_variables :in check-if-some-var-have-spaces :collect 
                (om::string+ 
                    (write-to-string (car all_variables)) " " 
                    (ompd-concatstring (mapcar   (lambda (x) (if 
                                                            (equal (type-of x) 'pathname)
                                                            (om::string+ (replace-all (namestring x) "\\" "/") " ")
                                                            (om::string+ (write-to-string x) " ")))
                                            (cdr all_variables))) " ")))
    (variaveis (ompd-concatstring (loop :for var :in make_var :collect (om::string+ " -send " (ompd-list->string-fun (list var))))))
    (pd-executable (pd patch))
    (pd-verbose (if verbose " " " -noverbose -d 0 "))
    (gui (if gui " " " -nogui"))
    (offline (if offline " -batch " ""))
    (pd-patch (let* (
                        (path (probe-file (pd-path patch)))
                        (length-of-path (length (om::string-to-list (namestring (pd-path patch)) " "))))
                        (if (> length-of-path 1)
                            (let* (
                                (message (om::om-print (format nil "The pathname in the ~d spaces in it, coping to temp-files." (pd-path patch)) "OM-pd"))
                                (copy-to-tmp-files (om::tmpfile "temp-patch.pd")))
                                (om-print "There is Spaces in your PD patch pathname" "WARNING")
                                (system::copy-file path copy-to-tmp-files)
                                (replace-all (namestring copy-to-tmp-files) "\\" "/"))
                          (replace-all (namestring path) "\\" "/"))))
    (command-line (om::string+ pd-executable  " -audiooutdev 0 " gui " " pd-verbose " " offline " -open " pd-patch " -send \"om-loadbang bang\"" variaveis fixed_outfile fixed_infile " " )))
    (oa::om-command-line command-line verbose)
    (if gui (om::om-print "Finish!" "PD"))
    (mp:process-run-function "Delete Files"
                 () 
                  (lambda () (if sound-in (system::delete-file tmp-infile-name))))
    sound-out))



; ============================== TO RUN PATCHES IN WSL (JUST FOR WINDOWS) ==================

(defmethod! wsl-pd~ ((patch pure-data) &key (sound-in nil) (sound-out nil) (var nil) (gui t) (offline nil) (verbose nil) (thread nil))
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp running it on WSL.")
:icon 'pd
:doc "This object is responsible for formatting a command line that will start and run PureData in WSL (just for Windows), what it always returns is the sound-out."


(let* (
      (sound-in (case (type-of sound-in)
                      (sound (if (null (om::file-pathname sound-in))
                                 (car (om::list! (save-temp-sounds sound-in (om::string+ "format-" (format nil "~7,'0D" (om-random 0 999999)) "-"))))
                                 (car (om::list! (om::file-pathname sound-in)))))
                      (pathname sound-in)
                      (string (probe-file sound-in))
                      (lispworks:simple-text-string (probe-file sound-in))
                      (null nil)

                      ))
      (sound-out (case (type-of sound-out)
                      (sound (if (null (om::file-pathname sound-out))
                                 (car (om::list! (save-temp-sounds sound-out (om::string+ "format-" (format nil "~7,'0D" (om-random 0 999999)) "-"))))
                                 (car (om::list! (om::file-pathname sound-out)))))
                      (pathname sound-out)
                      (string (probe-file sound-out))
                      (lispworks:simple-text-string (probe-file sound-out))
                      (null (om::tmpfile "sound.wav")))))

    (wsl-ckn-pd~ sound-in sound-out patch var gui offline verbose)
    sound-out))

;; ======================================================================================



(defun wsl-ckn-pd~ (sound-in sound-out patch var gui offline verbose)

;; Check if outfile have some space;;;
(let* (
        (pd-outfile (om::string-to-list (namestring sound-out) " "))
        (length-of-path (length pd-outfile)))
    (if (> length-of-path 1)
        (progn
                (om::om-message-dialog "The outfile pathname have spaces in it, it will not work")
                (om::abort-eval))
        nil))


;; Here is the real work

(let* (

    (check-if-some-var-have-spaces (loop :for all-var :in var 
                                         :for var-index :from 1 :to (length var)
                                         :collect (if (or (equal (type-of (car (cdr all-var))) 'pathname) (equal (type-of (car (cdr all-var))) 'string))
                                                    (let* (
                                                        (path (probe-file (car (cdr all-var))))
                                                        (length-of-path (length (om::string-to-list (namestring path) " "))))
                                                        (if (> length-of-path 1)
                                                            (let* (
                                                                (message (om::om-print (format nil "The pathname in the ~d spaces in it, coping to temp-files." (car all-var)) "OM-pd"))
                                                                (copy-to-tmp-files (om::tmpfile (om::string+ (write-to-string var-index) "." (car (last (om::string-to-list (namestring path) ".")))))))
                                                                (system::copy-file path copy-to-tmp-files)
                                                                (om::x-append (car all-var) copy-to-tmp-files))
                                                                (om::x-append (car all-var) path)))
                                                    all-var)))
    ;(verbose (print check-if-some-var-have-spaces))
    (outfile (replace-all (namestring sound-out) "\\" "/"))
    (tmp-infile-name (if sound-in (om::tmpfile (om::string+ (write-to-string (om::om-random 10000 99999)) "." (car (last (om::string-to-list (namestring sound-in) ".")))))))
    (int-copy-to-tmpfile (if sound-in (om::om-copy-file sound-in tmp-infile-name)))
    (fixed_infile (if sound-in (om::string+ " -send " (ompd-list->string-fun (list (om::string+ "infile " (path2wsl int-copy-to-tmpfile)))))))
    (fixed_outfile (if sound-out (om::string+ " -send " (ompd-list->string-fun (list  (om::string+ "outfile " (path2wsl outfile)))))))
    (make_var 
            (loop :for all_variables :in check-if-some-var-have-spaces :collect 
                (om::string+ 
                    (write-to-string (car all_variables)) " " 
                    (ompd-concatstring (mapcar   (lambda (x) (if 
                                                            (equal (type-of x) 'pathname)
                                                            (om::string+ (path2wsl x) " ")
                                                            (om::string+ (write-to-string x) " ")))
                                            (cdr all_variables))) " ")))
    (variaveis (ompd-concatstring (loop :for var :in make_var :collect (om::string+ " -send " (ompd-list->string-fun (list  var))))))
    (pd-executable (pd patch))
    (pd-verbose (if verbose " " " -noverbose -d 0 "))
    (gui (if gui " " " -nogui"))
    (offline (if offline " -batch " ""))
    (pd-patch (replace-all (namestring (pd-path patch)) "\\" "/"))
    (wsl-path (namestring (merge-pathnames "resources/executables/wsl/wsl.exe"  (mypathname (find-library "OM-pd")))))
    (command-line (om::string+ wsl-path " pd " " -audiooutdev 0 -blocksize 65536 -r 44100 -audiobuf 20000 -sleepgrain 200 " gui " " pd-verbose " " offline " -open " "'" (path2wsl pd-patch) "'" " -send \"om-loadbang bang\"" variaveis fixed_outfile fixed_infile " " )))
    (print command-line)
    (oa::om-command-line command-line verbose)
    (if gui (om::om-print "Finish!" "PD"))
    (mp:process-run-function "Delete Files"
                 () 
                  (lambda () (if sound-in (system::delete-file tmp-infile-name))))
    sound-out))


; =============================================== To Work with Multithreading

(defmethod! pd-mk-line ((patch pathname) &key (sound-in nil) (sound-out nil)  (var list) (gui t) (offline t) (verbose nil))
(pd-mk-line (pd-define-patch patch) :sound-in sound-in :sound-out sound-out :var var :gui gui :offline offline :verbose verbose))

; ========================

(defmethod! pd-mk-line ((patch string) &key (sound-in nil) (sound-out nil)  (var list) (gui t) (offline t) (verbose nil))
(pd-mk-line (pd-define-patch patch) :sound-in sound-in :sound-out sound-out :var var :gui gui :offline offline :verbose verbose))

; ===============================================
(defmethod! pd-mk-line ((patch pure-data) &key (sound-in nil) (sound-out nil)  (var list) (gui t) (offline t) (verbose nil))
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc "It works like pd~, but do not execute nothing, it just build the line to run pd-multithreading."


(mk-cmd-pd~ sound-in sound-out patch var gui offline verbose))


; ===============================================

(defun mk-cmd-pd~ (sound-in sound-out patch var gui offline verbose)

;; Check if outfile have some space;;;
(let* (
        (pd-outfile (om::string-to-list (namestring sound-out) " "))
        (length-of-path (length pd-outfile)))
    (if (> length-of-path 1)
        (progn
                (om::om-message-dialog "The outfile pathname have spaces in it, it will not work")
                (om::abort-eval))
        nil))


;; Here is the real work

(let* (

    (check-if-some-var-have-spaces (loop :for all-var :in var 
                                         :for var-index :from 1 :to (length var)
                                         :collect (if (or (equal (type-of (car (cdr all-var))) 'pathname) (equal (type-of (car (cdr all-var))) 'string))
                                                    (let* (
                                                        (path (probe-file (car (cdr all-var))))
                                                        (length-of-path (length (om::string-to-list (namestring path) " "))))
                                                        (if (> length-of-path 1)
                                                            (let* (
                                                                (message (om::om-print (format nil "The pathname in the ~d spaces in it, coping to temp-files." (car all-var)) "OM-pd"))
                                                                (copy-to-tmp-files (om::tmpfile (om::string+ (write-to-string var-index) "." (car (last (om::string-to-list (namestring path) ".")))))))
                                                                (system::copy-file path copy-to-tmp-files)
                                                                (om::x-append (car all-var) copy-to-tmp-files))
                                                                (om::x-append (car all-var) path)))
                                                    all-var)))
    ;(verbose (print check-if-some-var-have-spaces))
    (outfile (replace-all (namestring sound-out) "\\" "/"))
    (tmp-infile-name (if sound-in (om::tmpfile (om::string+ (write-to-string (om::om-random 10000 99999)) "." (car (last (om::string-to-list (namestring sound-in) ".")))))))
    (int-copy-to-tmpfile (if sound-in (om::om-copy-file sound-in tmp-infile-name)))
    (fixed_infile (if sound-in (om::string+ " -send " (ompd-list->string-fun (list (om::string+ "infile " (replace-all (namestring int-copy-to-tmpfile) "\\" "/") ", "))))))
    (fixed_outfile (if sound-out (om::string+ " -send " (ompd-list->string-fun (list (om::string+ "outfile " (replace-all (namestring outfile) "\\" "/") ", "))))))
    (make_var 
            (loop :for all_variables :in check-if-some-var-have-spaces :collect 
                (om::string+ 
                    (write-to-string (car all_variables)) " " 
                    (ompd-concatstring (mapcar   (lambda (x) (if 
                                                            (equal (type-of x) 'pathname)
                                                            (om::string+ (replace-all (namestring x) "\\" "/") " ")
                                                            (om::string+ (write-to-string x) " ")))
                                            (cdr all_variables))) " ")))
    (variaveis (ompd-concatstring (loop :for var :in make_var :collect (om::string+ " -send " (ompd-list->string-fun (list var))))))
    (pd-executable (pd patch))
    (pd-verbose (if verbose " " " -noverbose -d 0 "))
    (gui (if gui " " " -nogui"))
    (offline (if offline " -batch " ""))
    (pd-patch (let* (
                        (path (probe-file (pd-path patch)))
                        (length-of-path (length (om::string-to-list (namestring path) " "))))
                        (if (> length-of-path 1)
                            (let* (
                                (message (om::om-print (format nil "The pathname in the ~d spaces in it, coping to temp-files." (pd-path patch)) "OM-pd"))
                                (copy-to-tmp-files (om::tmpfile "temp-patch.pd")))
                                (system::copy-file path copy-to-tmp-files)
                                (replace-all (namestring copy-to-tmp-files) "\\" "/"))
                             (namestring path))))
    (command-line (om::string+ pd-executable  " -audiooutdev 0 " gui " " pd-verbose " " offline " -open " pd-patch " -send \"om-loadbang bang\"" variaveis fixed_infile fixed_outfile " " )))

(om::make-value 'pure-data (list (list :command-line command-line) (list :pd-outfile outfile)))))


; ================================================================

(defmethod! pd-patches-list (&key (show-all-patches nil))
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc "This function return a list of all the patches in the pd-patches folder."
(let* (
        (thepath (get-pref-value :externals :Pd-Patches))
        (thefilelist (search-patches "pd")))
        (remove nil 
            (loop for patches in thefilelist 
                :collect (let* (
                    (name-of-patch (ompd-get-filename patches))
                    (check-if-is-children (car (string-to-list name-of-patch "_"))))
                    
        (if show-all-patches                  
            (ompd-get-filename patches)

            (if (not (equal check-if-is-children "Children"))
                        (ompd-get-filename patches))))))))


; ================================================================
(defmethod! pd-define-patch ((names-of-patch list))
(loop for strings in names-of-patch :collect (pd-define-patch strings)))

; ================================================================
(defmethod! pd-define-patch ((name-of-patch string))
(let* (
(action1 (probe-file (merge-pathnames name-of-patch (namestring (get-pref-value :externals :Pd-Patches))))))
(if 
    (equal nil action1) 
        (let* (
                (action1 (om-print "PD-Patch not found" "Warning")))
                (om::make-value 'pure-data (list (list :pd-path " "))))
    (om::make-value 'pure-data (list (list :pd-path action1))))))


; ====================
(defmethod! pd-define-patch ((name-of-patch pathname))
:initvals '(nil)
:indoc '("Search pd patches using the name of the patch") 
:icon 'pd
:doc "It search the patch using the name of the patch, if the patch is not found it will return a warning."

(let* (
(action1 (probe-file (merge-pathnames name-of-patch (namestring (get-pref-value :externals :Pd-Patches))))))
(if 
    (equal nil action1) 
        (let* (
                (action1 (om-print "PD-Patch not found" "Warning")))
                (om::make-value 'pure-data (list (list :pd-path " "))))
    (om::make-value 'pure-data (list (list :pd-path action1))))))


; ================================================================

(defun search-patches (extension)

      (let* (
            (thepath (get-pref-value :externals :Pd-Patches))
            (thefilelist (om-directory thepath 
                                                :type extension
                                                :directories t 
                                                :files t 
                                                :resolve-aliases nil 
                                                :hidden-files nil))

            (action1 
                  (loop :for loop-files :in thefilelist 
                        :collect (if 
                                          (system::directory-pathname-p loop-files)
                                          (ompd-search-inside-some-folder loop-files extension)
                                          loop-files))))
            (remove nil (flat action1))))

; ================================================================

(defmethod! pd-multithreading ((patch-list list) (patches-by-thread number))
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc "This function will run the pd patches in a multithreading way, the number of threads is defined by the user. For example, with 20 tasks, if the user define 4 threads, each thread will run 5 tasks. The function will return a list with the results of each thread."


(if (not (find-library "OM-CKN"))
    (progn  
                   (if (om-y-or-n-dialog "Pd-multithreading require the library OM-CKN, want to download it?")
                       (hqn-web:browse "https://github.com/charlesneimog/OM-CKN/releases/"))
                   (abort-eval)))
    

(let* (
      (patches-by-thread (ckn-loop-multi-prepare patch-list patches-by-thread))
      (thread (lambda (x) (loop :for patches :in x :collect (progn (oa::om-command-line (om::command-line patches)) (pd-outfile patches))))))
      (ckn-multi-1-var thread patches-by-thread)
      (mapcar (lambda (out) (pd-outfile out)) patch-list)))
      


;; ================================================================

(defmethod! pd-open-patches ((patch pathname))
(pd-open-patches (pd-define-patch patch)))

;; ================================================================

(defmethod! pd-open-patches ((patch string))
(pd-open-patches (pd-define-patch patch)))

;; ================================================================

(defmethod! pd-open-patches ((patch pure-data))
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc "This function will open the patch in the pd application."

(mp:process-run-function "Open PureData"
                 () 
                  #-windows(lambda () (om-cmd-line (om::string+ (pd patch) " -d 0 -audiooutdev 0 " (replace-all (namestring (pd-path patch)) "\\" "/"))))
                  #+windows(lambda () (om-cmd-line (om::string+ (pd patch) " -d 0 -audiooutdev 0 " (ompd-list->string-fun (list (replace-all (namestring (pd-path patch)) "\\" "/"))))))
              ))

;; ================================================================

(defmethod! pd-wait-process ()
:initvals '(nil)
:indoc ' ("It needs to be used with finish-process!")
:icon 'pd
:doc "This function will wait the process to finish."

(setf *pd-wait-process* nil)

(om-start-udp-server 1012 "127.0.0.1" (lambda (msg) (let () (if  (equal (car (cdr (osc-decode msg))) 1.0)
                                                                              (progn (setf *pd-wait-process* t) nil)
                                                                              )))) ;;; When PD is open, the loadbang will sent one 1.0 to this port.

(loop :with pd-start = nil 
      :while (null *pd-wait-process*)
      :finally (om::om-print "Finished process!" "PD")) ;; Wait PD to be open!


(loop :for udp-server :in *running-udp-servers*
      :do (if (equal (mp:process-name (third udp-server)) "UDP receive server on \"127.0.0.1\" 1012")
              (progn (om::om-stop-udp-server (third udp-server)))))

    t)
