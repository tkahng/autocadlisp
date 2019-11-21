;;;https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/deleting-empty-layouts/m-p/3352805/highlight/true#M302583


(defun C:DAEL (); = Delete All Empty Layouts
  (setvar 'ctab "Model")
  (foreach layout (layoutlist)
    (if
      (= (sslength (ssget "_X" (list (cons 410 layout)))) 1); nothing in it but its paper space viewport
      (command "_.layout" "_delete" layout)
    ); if
  ); foreach
); defun