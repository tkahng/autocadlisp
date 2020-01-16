(defun c:DimUpd ( / *error* _StartUndo _EndUndo doc ss )
  (vl-load-com)

  (defun *error* ( msg )
    (and doc (_EndUndo doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (if (ssget "_X" '((0 . "*DIMENSION")))
    (progn
      (setq *scl*
        (cond
          (
            (getdist
              (strcat "\nSpecify Linear Scale Factor <"
                (rtos
                  (setq *scl* (cond ( *scl* ) ( 1.0 )))
                )
                "> : "
              )
            )
          )
          ( *scl* )
        )
      )
      (setq *oscl*
        (cond
          (
            (getdist
              (strcat "\nSpecify Overall Scale Factor <"
                (rtos
                  (setq *oscl* (cond ( *oscl* ) ( 1.0 )))
                )
                "> : "
              )
            )
          )
          ( *oscl* )
        )
      )
      
      (_StartUndo doc)
      
      (vlax-for o (setq ss (vla-get-ActiveSelectionSet doc))
        (mapcar
          (function
            (lambda ( prop value )
              (if (vlax-property-available-p o prop)
                (vlax-put-property o prop value)
              )
            )
          )
          '(LinearScaleFactor ScaleFactor) (list *scl* *oscl*)
        )
      )

      (vla-delete ss)

      (_EndUndo doc)
    )
  )

  (princ)
)