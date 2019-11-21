;;;https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/changespace-everything-from-pspace-to-mspace/m-p/7081403#M353221


(defun c:chsm  (/ ss1)
 (mapcar '(lambda (x)
           (setvar 'ctab x)
           (command-s "._pspace")
           (if (setq ss1 (ssget "_x" (list (cons 410 x) '(-4 . "<not") '(0 . "viewport") '(-4 . "not>"))))
            (command-s "._chspace" ss1 ""  "")))
         (layoutlist))
 (princ))