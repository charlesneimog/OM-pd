;;            OM-pd
;;
;;      by Charles K. Neimog 
;; collab with reddit users 
;; University of São Paulo (2021-2022)
           

(in-package :om)

(defun lib-src-file (file)
  (merge-pathnames file (om-make-pathname :directory *load-pathname*)))

(mapcar #'(lambda (file) (compile&load (lib-src-file file) t t))
      '(
            "Sources/utilities"
            "Sources/pd"
            "Sources/om6-pref"
            "Sources/multithreading"
            ))

(om::fill-library '((nil nil nil (pd~ pd-open-patches pd-mk-line pd-patches-list pd-define-patch pd-multithreading) nil)))


(print 
 "
                                              OM-pd

      by Charles K. Neimog | charlesneimog.com   
      University of São Paulo (2021)
"
)
                    

