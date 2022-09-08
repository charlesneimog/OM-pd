;;            OM-JI
;;
;;      by Charles K. Neimog 
;; collab with reddit users 
;; University of São Paulo (2020-2021)
           

(in-package :om)

(defun lib-src-file (file)
  (merge-pathnames file (om-make-pathname :directory *load-pathname*)))

(mapcar #'(lambda (file) (compile&load (lib-src-file file) t t))
      '(
            "Sources/utilities"
            "Sources/pd"
            "Sources/om6-pref"
            ))


(om::fill-library '(
      ("OM-pd" nil nil (pd~ pd-open-patches pd-mk-line pd-patches-list pd-define-patch pd-multithreading) nil)))



(print 
 "
                                              OM-JI

      by Charles K. Neimog | charlesneimog.com  
            collab with reddit users 
      Universidade Federal de Juiz de Fora (2019-2020)
"
)
                    

