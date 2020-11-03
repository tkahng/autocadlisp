;;;https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/batch-plot-to-pdf-in-model-space/m-p/6220396#M339775
(vl-load-com)
(defun c:plotblock (/ dwg file hnd i len llpt lst mn mx ss tab urpt)
    (if (setq ss (ssget '((0 . "INSERT"))))
        (progn
            (repeat (setq i (sslength ss))
                (setq hnd (ssname ss (setq i (1- i)))
                      tab (cdr (assoc 410 (entget hnd)))
                      lst (cons (cons tab hnd) lst)
                )
            )
            (setq lst (vl-sort lst '(lambda (x y) (> (car x) (car y)))))
            (setq i 0)
            (foreach x lst
                (setq file (strcat (getvar 'DWGPREFIX)
                                   (substr (setq dwg (getvar 'DWGNAME)) 1 (- (strlen dwg) 4))
                                   "-"
                                   (itoa (setq i (1+ i)))
                                   ".pdf"
                           )
                )
                (if (findfile file)
                    (vl-file-delete file)
                )
                (vla-getboundingbox (vlax-ename->vla-object (cdr x)) 'mn 'mx)
                (setq llpt (vlax-safearray->list mn)
                      urpt (vlax-safearray->list mx)
                      len  (distance llpt (list (car urpt) (cadr llpt)))
                )
                (command "-plot"
                         "yes"
                         (car x)
                         "DocuWorks Printer"
                        ;  "A4 (210x297mm)"
                         "A3 (297x420mm)"
                         "Millimeters"
                         "Landscape"
                         "No"
                         "Window"
                         llpt
                         urpt
                         "1=200"
                         "Center"
                         "yes"
                        ;  "jb.ctb"
                         "소방기계A3.ctb"
                         "yes"
                         "As displayed"
                         "No"
                         "No"
                         "Yes"
                )
            )
        )
    )
    (princ)
)