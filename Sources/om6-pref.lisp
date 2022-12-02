(in-package :om)


#-Linux(defvar *PD-EXE* "Path to PureData executable")
(defvar *PD-PATCHES* nil)


(if (not *PD-PATCHES*)
    (let* (
            (om-pd-library (lib-pathname (find-library "om-pd"))))
            (setf *PD-PATCHES* (om-make-pathname :directory (append (pathname-directory om-pd-library)  '("resources" "Pd-Patches"))      
                                                :host (pathname-host om-pd-library) :device (pathname-device om-pd-library)))))

#-Linux(pushr 'pd-exe *external-prefs*)
(pushr 'pd-patches *external-prefs*)

#-Linux(defmethod get-external-name ((module (eql 'pd-exe))) "PureData Executable")
(defmethod get-external-name ((module (eql 'pd-patches))) "Folder to PureData Patches")

#-Linux(defmethod get-external-module-path ((module (eql 'pd-exe)) modulepref) (get-pref modulepref :pd-exe))
(defmethod get-external-module-path ((module (eql 'pd-patches)) modulepref) (get-pref modulepref :pd-patches))

#-Linux(defmethod set-external-module-path ((module (eql 'pd-exe)) modulepref path) (set-pref modulepref :pd-exe path))

(defmethod set-external-module-path ((module (eql 'pd-patches)) modulepref path) (set-pref modulepref :pd-patches path))

#-Linux(defun set-pd-path () (set-pref (find-pref-module :externals) :pd-exe (pathname nil)))
(defun set-pd-patches-path () (set-pref (find-pref-module :externals) :pd-patches nil))


#-Linux(defmethod save-external-prefs ((module (eql 'pd-exe))) `(:pd-exe ,(om-save-pathname *PD-EXE*)))
(defmethod save-external-prefs ((module (eql 'pd-patches))) `(:pd-patches *PD-PATCHES*))


#-Linux(defmethod put-external-preferences ((module (eql 'pd-exe)) moduleprefs) (when (get-pref moduleprefs :pd-exe) (setf *PD-EXE* (find-true-external (get-pref moduleprefs :pd-exe)))) t)
#+Linux(setf *PD-EXE* "pd ")



(defmethod put-external-preferences ((module (eql 'pd-patches)) moduleprefs)
    (when (get-pref moduleprefs :pd-patches)
            (setf *PD-PATCHES* (get-pref moduleprefs :pd-patches)))
            t)