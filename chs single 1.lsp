;;;https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/changespace-everything-from-pspace-to-mspace/m-p/7081847/highlight/true#M353225


(defun c:chss ( / )
  (command "._tilemode" "0")
  (command "._pspace")
  (command "._chspace" "all" "")
  (command "._tilemode" "1")
  (princ)
  )