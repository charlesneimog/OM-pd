(in-package :om)

;===================================================================== Files control =====================================

(defun get-filename (x)
"I forgot the name of the function that I stolen somewhere."

(let* (
      (filename (namestring x))
                     #+Windows(file (om::string-to-list filename "\\"))
                     #-Windows(file (om::string-to-list filename "/"))
                                                                                                                       )

  (car (last file))))   

;=====================================================================  

(defun search-inside-some-folder (folder extension)                                                                         
      (let* (
            (thepath folder)
            (thefilelist (om-directory thepath 
                              :type extension
                              :directories t 
                              :files t 
                              :resolve-aliases nil 
                              :hidden-files nil))
            (more-folders? (mapcar (lambda (x) (if 
                                                      (system::directory-pathname-p x)
                                                      (search-inside-some-folder x extension)
                                                      x)) thefilelist)))
            more-folders?))

;=====================================================================  

(defun list->string-fun (ckn-list)
  (when ckn-list
    (concatenate 'string 
                 (write-to-string (car ckn-list)) (list->string-fun (cdr ckn-list)))))


;; ======================================
(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))
        
      ;; https://stackoverflow.com/questions/5457346/lisp-function-to-concatenate-a-list-of-string

;=====================================================================  

