(defun bg:block-get-name (blockentity / name repbtag )
  (if (eq (type blockentity) 'VLA-OBJECT)(setq blockentity (vlax-vla-object->ename blockentity)))
;;;get from Lee Mac  LM:EffectiveName
;;; http://www.theswamp.org/index.php?topic=37493.0
;;; http://forum.dwg.ru/showthread.php?t=65082
;;----------------=={ Effective Block Name }==----------------;;
;;                                                            ;;
;;  Returns the effective name of a block.                    ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  blockentity - Block Reference Entity name                 ;;
;;------------------------------------------------------------;;
;;  Returns:  True block name as per the block definition     ;;
;;------------------------------------------------------------;;
 
  (if (wcmatch (setq name (cdr (assoc 2 (entget blockentity)))) "`**")
    (if
      (and
        (setq repbtag
          (cdadr
            (assoc -3
              (entget
                (cdr
                  (assoc 330
                    (entget (tblobjname "BLOCK" name))
                  )
                )
               '("AcDbBlockRepBTag")
              )
            )
          )
        )
        (setq repbtag (handent (cdr (assoc 1005 repbtag))))
      )
      (setq name (cdr (assoc 2 (entget repbtag))))
    )
  )
  name
)

(defun bg:block-GetXclip ( vla-obj / result )

  
   (vl-catch-all-apply
      '(lambda ( )
           (if
               (and
		 (eq (vla-get-ObjectName vla-obj) "AcDbBlockReference")
                   (eq :vlax-true
                       (vla-get-HasExtensionDictionary vla-obj)
                   )
               )
               (setq result
                   (entget
                       (vlax-vla-object->ename
                           (vla-item
                               (vla-item
                                   (vla-getExtensiondictionary vla-obj)
                                   "ACAD_FILTER"
                               )
                               "SPATIAL"
                           )
                       )
                   )
               )
           )
       )
   )

   result
)

(defun c:BGBLXCLIPEXP( / ent)
  (vl-load-com)
  ((lambda(ent actdoc)
     (vla-startundomark actdoc)
     (if (and ent
              (not (vl-catch-all-error-p ent))
              )
       ((lambda(obj point)
          (if (bg:block-GetXclip (vlax-ename->vla-object obj))
            (bg:block-xclip-exp obj point)
            (prompt (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\n��������� ���� �� ��������" "\nThe specified block is not clipped"))
            )
          )
         (car ent)
         (cadr ent)
         )
       )
     (vla-endundomark actdoc)
     )
    (vl-catch-all-apply 'entsel
                        (list (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\n������� ����������� ����: " "\nSelect clipped block"))
                        )
    (vla-get-activedocument (vlax-get-acad-object))
    )
  (princ)
  ); end c:BGBLXCLIPEXP

(defun bg:block-xclip-exp(blk point / pline_obj *error*)
  (defun *error* (msg)
    (princ msg)
    (mapcar '(lambda (x)(if(vlax-write-enabled-p x)(vla-put-visible x :vlax-true))) hiden) 
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
      )

  (if (vl-cmdf "_.xclip" blk "" "_P")
    ((lambda(pline_obj)
       (vl-cmdf "_.zoom" "_O" pline_obj "")
       ((lambda(lst_exp_obj lst_xclip eps)
          (if (and lst_exp_obj
                   lst_xclip
                   )
            ((lambda(is_int_point)
               ((lambda(nbr_xclip pline_trim / i e1)
                  (setq i 0 ss (ssadd))
                  (while (< i (sslength nbr_xclip))
                    (if (not(vl-position(vlax-ename->vla-object(setq e1(ssname nbr_xclip i))) lst_exp_obj))
                      (progn
                        (setq hiden (cons (vlax-ename->vla-object e1) hiden))
                        (ssdel e1 nbr_xclip)
                        )
                      (setq i (1+ i))
                      )
                    )
                   (mapcar '(lambda (x)(if(vlax-write-enabled-p x)(vla-put-visible x :vlax-false))) hiden)
                  (VL-CATCH-ALL-APPLY '(lambda()
                  (if (and nbr_xclip pline_trim)
                    (progn
                      (foreach item (mapcar 'vlax-vla-object->ename lst_exp_obj)
                        (if is_int_point
                          (if (not (ssmemb item nbr_xclip))
                            (entdel item)
                            )
                          (if (ssmemb item nbr_xclip)
                            (entdel item)
                            )
                          )
                        )
                      (apply 'vl-cmdf (append (list "_.trim" pline_obj "" "_F")
                                              ((lambda(ed_pline_trim)
                                                 (append (mapcar 'cdr (vl-remove-if-not (function (lambda(x) (= (car x) 10)))
                                                                                        ed_pline_trim
                                                                                        )
                                                                 )
                                                         (list (cdr (assoc 10 ed_pline_trim)))
                                                         )
                                                 )
                                                (entget pline_trim)
                                                )
                                              (list "" "")
                                              )
                             )
                      (entdel pline_trim)
                      )
                    )
                    ))
                  (mapcar '(lambda (x)(if(vlax-write-enabled-p x)(vla-put-visible x :vlax-true))) hiden) 
                  (vl-cmdf "_.zoom" "_P")
                  )
                 (if is_int_point
                   (ssget "_CP" lst_xclip)
                   (ssget "_WP" lst_xclip)
                   )
                 ((lambda(reverse-point)
                    (if reverse-point
                      (if (vl-cmdf "_.offset" eps pline_obj (get-reverse-point point pline_obj 0.1) "")
                        (entlast)
                        )
                      )
                    )
                   (get-reverse-point point pline_obj 0.1)
                   )
                 )
               )
              ((lambda(point_obj / result)
                 (setq result (ssmemb point_obj (ssget "_CP" lst_xclip)))
                 (entdel point_obj)
                 result
                 )
                (entmakex (list '(0 . "POINT") (cons 10 point)))
                )
              )
            )
          )
         (bg:burst-list blk)
         (mapcar 'cdr (vl-remove-if-not (function (lambda(x) (= (car x) 10))) (entget pline_obj)))
         (min 0.1 (* (getvar 'viewsize) 0.01))
         )
       (entdel pline_obj)
       (vl-cmdf "_.zoom" "_P")
       )
      (entlast)
      )
    )
  ); end bg:block-xclip-exp

(defun get-reverse-point(pt obj e / )
  ((lambda(cl_pt)
     (if cl_pt
       ((lambda(param_cl_pt end_param)
          (if param_cl_pt
            ((lambda(p1 p2)
               (if (not p1) (setq p1 (vlax-curve-getPointAtParam obj e)))
               (if (not p2) (setq p2 (vlax-curve-getPointAtParam obj (- end_param e))))
               ((lambda(c_p)
                  (polar pt (angle pt c_p) (+ (distance pt c_p) e))
                  )
                 (polar p1 (angle p1 p2) (* (distance p1 p2) 0.5))
                 )
               )
              (vlax-curve-getPointAtParam obj (+ param_cl_pt e))
              (vlax-curve-getPointAtParam obj (- param_cl_pt e))
              )
            )
          )
         (vlax-curve-getParamAtPoint obj cl_pt)
         (vlax-curve-getEndParam obj)
         )
       )
     )
    (vlax-curve-getClosestPointTo obj pt)
    )
  ); end get-reverse-point

(defun bg:burst-list (blk / ret ELAST ENAME ENT)
  (if (= (type blk) 'VLA-OBJECT)(setq blk (vlax-vla-object->ename blk)))
  (Setq ELAST (LASTENT))
  (bg:BURST-ONE blk)
  (setq ENAME ELAST ret nil)
  (While (Setq ENAME (EntNext ENAME))
    (setq ENT (entget ENAME))
    (if
      (and (assoc 60 ENT);_(check Visible dxf code 60) if code 60 set to 1 - object invisible
            (= 1 (cdr(assoc 60 ENT)))
           )
         (VL-CATCH-ALL-APPLY 'entdel (list ENAME))
      (setq ret (cons (vlax-ename->vla-object ENAME) ret))
       )
    )
  ret)

(defun bg:explode-block ( blk level / adoc csp blk_obj)

  (if (vlax-write-enabled-p (setq blk_obj (if (= (type blk) 'ENAME)(vlax-ename->vla-object blk) blk)))
    (progn
      (if
      (and (vlax-property-available-p blk_obj 'isdynamicblock)
	(= (vla-get-isdynamicblock blk_obj) :vlax-true)
	)
       (bg:DynToStatic blk_obj nil)
      )
      (foreach memb (bg:burst-list Blk)
        (cond ((and
                 level
                 (= (vla-get-ObjectName memb) "AcDbBlockReference")
                 )
               (bg:explode-block (vlax-vla-object->ename memb) level ));_BURST блокам
	(t  nil )
         )
        )
  )
      )
    
  )

(Defun bg:BURST-ONE (BNAME	   /	BBLOCK    BENT     ANAME    ENT      ATYPE
		  AENT	   AGAIN    ENAME    ENT      SS-COLOR SS-LAYER
		  SS-LTYPE SS-LWEIGHT  mirror   ss-mirror TMP mlast BLAYER  BCOLOR BLTYPE BLWEIGHT
                  attlist BOBJ _ITEM _bump _ATT-TEXT SS-ATTR
		 )
;;;********* Ф-ции выдраны из BURST.LSP ************
;;; Наличие Express обязательна !!!
(Defun _ITEM (N E) (CDR (Assoc N E)))
;;;(acet-error-init (list (list "cmdecho" 0
;;;"highlight" 1) T))
;;;(Defun _BITSET (A B) (= (Boole 1 A B) B))
(Defun _bump (prmpt)(GRTEXT -2 prmpt))
(Defun _ATT-TEXT (AENT / ANAME TENT ILIST INUM lineweight)
      (setq ANAME (cdr (assoc -1 AENT)))
      (if (and _MATTS_UTIL (_MATTS_UTIL ANAME)) 
         (progn
            ; Multiple Line Text Attributes (MATTS) -
            ; make an MTEXT entity from the MATTS data
            (_MATTS_UTIL ANAME 1)
         )
         (progn
            ; else -Single line attribute conversion
            (Setq TENT '((0 . "TEXT")))
            (ForEach INUM '(8 6 38 39 62 67 210 10 40 1 50 41 51 7 71 72 73 11 74)
               (If (Setq ILIST (Assoc INUM AENT))
                   (Setq TENT (Cons ILIST TENT))
               )
            )
            (Setq
               tent (Subst
                       (Cons 73 (_ITEM 74 aent))
                       (Assoc 74 tent)
                       tent
                    )
            )
;;; VVA ADD 2011-10-26 BEGIN
           ((lambda ( itm / ed next)
              (setq next t)
              (while (and next (setq itm (entnext itm)))
                (setq ed (entget itm))
                (if (and (= (_ITEM 0 ed) "ATTDEF")
                         (= (_ITEM 2 ed)(_ITEM 2 AENT))
                         )
                  (setq next nil lineweight (_ITEM 370 ed))
                  )
                )
              )
             (tblobjname "BLOCK" (bg:block-get-name (cdr(assoc 330 AENT))))
             )
           (cond
             ((= lineweight '-3) ;_default lineweight
              (setq TENT (cons (cons 370 (getvar "LWDEFAULT")) TENT))
              )
             ((= lineweight '-2) ;_byblock lineweight
              (if (setq lineweight  (cdr(assoc 370 (entget(cdr(assoc 330 AENT))))))
                (setq TENT (cons (cons 370 lineweight) TENT))
                ((lambda ( lw )
                   (cond
                     ((= lw '-3)
                      (setq TENT (cons (cons 370 (getvar "LWDEFAULT")) TENT))
                      )
                     ((numberp lw)
                      (setq TENT (cons (cons 370 lw) TENT))
                      )
                     (t nil)
                     )
                   )
                  (cdr(assoc 370 (entget(TBLOBJNAME "LAYER" (cdr(assoc 8 (entget(cdr(assoc 330 AENT)))))))))
                  )
                )
              )
             ((and lineweight (not (minusp lineweight)))
              (setq TENT (cons (cons 370 lineweight) TENT))
              )
             (t ;_bylayer
              ((lambda ( lw )
                   (cond
                     ((= lw '-3)
                      (setq TENT (cons (cons 370 (getvar "LWDEFAULT")) TENT))
                      )
                     ((numberp lw)
                      (setq TENT (cons (cons 370 lw) TENT))
                      )
                     (t nil)
                     )
                   )
                  (cdr(assoc 370 (entget(TBLOBJNAME "LAYER" (cdr(assoc 8 (entget(cdr(assoc 330 AENT)))))))))
                  )
              )
             )
             
;;; VVA ADD 2011-10-26 END
              
            (EntMakex (Reverse TENT))
         )
      )
   ) ;_ end of Defun

  
  (Setq	BENT   (EntGet BNAME)
        BOBJ   (vlax-ename->vla-object BNAME)
	BLAYER (vla-get-Layer BOBJ)
	BCOLOR (vla-get-Color BOBJ)
	BBLOCK (bg:block-get-name BNAME) ;;; (_ITEM 2 BENT)
	BLTYPE (vla-get-linetype BOBJ)
	BLWEIGHT (vla-get-LineWeight BOBJ)
        SS-ATTR (ssadd)
  )
  (Setq ELAST (LASTENT) attlist nil)
  (if (bg:EXPLODABLE BBLOCK)
    (progn
  (If (= 1 (_ITEM 66 BENT))
    (Progn (Setq ANAME BNAME)
	   (While (Setq	ANAME (EntNext ANAME)
			AENT  (EntGet ANAME)
			ATYPE (_ITEM 0 AENT)
			AGAIN (= "ATTRIB" ATYPE)
		  )
;;;	     (_bump "Converting attributes")
;;;-> VVA 30.07.2008
             (if (or (null(_ITEM 60 AENT))
                     (/= (_ITEM 60 AENT) 1) ;_ Visible ON (check dxf code 60) if code 60 set to 1 - object invivible
                     )
             (if (or
                     (zerop (logand (_ITEM 70 AENT) 1)) ;_Change by VVA Attr fix 03.09.2008 Not hidden attribute
                     (zerop (logand (_ITEM 70 AENT) 9)) ;_Add kakt00z 1.06.2010 ( http://forum.dwg.ru/showpost.php?p=580531&postcount=33 )
                     )
	       (progn
		  (setq tmp (cdr (assoc 2 AENT)))           ;_ add VVA 20.03.2009
	          (_ATT-TEXT AENT)                           ;_Change by VVA Attr fix 03.09.2008
		  (setq attlist (cons (list tmp (entlast)) attlist)) ;_ add VVA 20.03.2009
		  )
	     )
               )
	   )
    )
  )
;;;<- VVA 30.07.2008
  (_bump (strcat "Exploding block " BBLOCK))
;;; Сначала пытаемся взорвать методом vla-explode
  (if (VL-CATCH-ALL-ERROR-P
        (VL-CATCH-ALL-APPLY 'vla-Explode (list(vlax-ename->vla-object BNAME)))
        )
    (acet-explode BNAME)
    (VL-CATCH-ALL-APPLY 'vla-delete (list(vlax-ename->vla-object BNAME)))
    )
  (Setq	SS-LAYER nil
	SS-COLOR nil
	SS-LTYPE nil
	SS-LWEIGHT nil    ;_Add VVA BURST LWEIGHT SECTION
	ENAME	 ELAST
  )
;;;  (_bump "Gathering pieces")
  (While (Setq ENAME (EntNext ENAME))
    (Setq ENT	(EntGet ENAME)
	  ETYPE	(_ITEM 0 ENT)
    )
    (cond
      ((= "ATTDEF" ETYPE)
	(setq tmp (cdr(assoc 2 ENT)))   ;_ add VVA 20.03.2009
       (if (cadr (assoc tmp attlist))
          (ssadd (cadr (assoc tmp attlist)) SS-ATTR)
         )
        (setq attlist (bg:del-from-list tmp attlist))   ;_ add VVA 13.07.2010
;;;               (If (_BITSET (_ITEM 70 ENT) 2)   ;_Rem by VVA Attr fix   03.09.2008
;;;                  (ATT-TEXT ENT)              ;_Rem by VVA Attr fix   03.09.2008
;;;               )                              ;_Rem by VVA Attr fix   03.09.2008
        (EntDel ENAME)
       )
      ((= "SEQEND" ETYPE) nil)
      ((and (_ITEM 60 ENT);_(check Visible dxf code 60) if code 60 set to 1 - object invisible
            (= 1 (_ITEM 60 ENT))
            )
         (VL-CATCH-ALL-APPLY 'entdel (list ENAME))
       )
      (t ;_Other entyties
       (If (= "0" (_ITEM 8 ENT))
	      (setq SS-LAYER (cons ENAME SS-LAYER))
	     )
	     (If (= 0 (_ITEM 62 ENT))    ;_ -> START Change VVA BURST
	       (if (= "0" (_ITEM 8 ENT))
		 (setq SS-COLOR (cons ENAME SS-COLOR))
		 (progn
		   (if (null (_ITEM 62 BENT)) ;_Block color type bylayer
		    ;;;(command "_.chprop" ENAME "" "_C" (_ITEM 62 (entget (TBLOBJNAME "LAYER" BLAYER))) "")
                     (bg:change-prop ENAME "Color" (_ITEM 62 (entget (TBLOBJNAME "LAYER" BLAYER))))
		    (setq SS-COLOR (cons ENAME SS-COLOR))
		     )
		   )
		 )                     ;_ <- END Change VVA BURST
	     )
	     (If (= "BYBLOCK" (strcase (cond ((_ITEM 6 ENT))("")))) ;_ -> START Change VVA BURST
	       (if (= "0" (_ITEM 8 ENT))
		 (setq SS-LTYPE (cons ENAME SS-LTYPE))
		 (progn
		   (if (= "BYLAYER" (strcase (cond ((_ITEM 6 BENT))("BYLAYER")))) ;_Block line type bylayer
		    ;;;(command "_.chprop" ENAME "" "_LT" (_ITEM 6 (entget (TBLOBJNAME "LAYER" BLAYER))) "")
                     (bg:change-prop ENAME "Linetype" (_ITEM 6 (entget (TBLOBJNAME "LAYER" BLAYER))))
		    (setq SS-LTYPE (cons ENAME SS-LTYPE) )
		     )
		   )
		 )
	     )                                                     ;_ <- END Change VVA BURST
	;_ -> START Change VVA BURST LWEIGHT SECTION
	(If (= -2 (_ITEM 370 ENT)) ;_ -> BYBLOCK LWEIGHT
	       (if (= "0" (_ITEM 8 ENT))
		 (setq SS-LWEIGHT (cons ENAME SS-LWEIGHT))
		 (progn
		   (if (NOT (_ITEM 370 BENT)) ;_Block LWEIGHT BYLAYER
                    (bg:change-prop ENAME "LineWeight" (vla-get-LineWeight (vlax-ename->vla-object (TBLOBJNAME "LAYER" BLAYER))))
		    (setq SS-LWEIGHT (cons ENAME SS-LWEIGHT))
		     )
		   )
		 )
	     )
	;_ <- END Change VVA BURST LWEIGHT SECTION
      
       
       )
      )
  )
  (foreach itm attlist                 ;_ add VVA 20.03.2009
       (if itm (progn                     ;_ add VVA 20.03.2009
		 (setq tmp (cadr itm))    ;_ add VVA 20.03.2009
		 (setq SS-LAYER (VL-REMOVE tmp SS-LAYER))     ;_ add VVA 20.03.2009
		 (setq SS-COLOR (VL-REMOVE tmp SS-COLOR))     ;_ add VVA 20.03.2009
		 (setq SS-LTYPE (VL-REMOVE tmp SS-LTYPE))     ;_ add VVA 20.03.2009
		 (setq SS-LWEIGHT (VL-REMOVE tmp SS-LWEIGHT))   ;_ add VVA 20.03.2009
                 (if (= (type tmp) 'ENAME)
		 (ENTDEL tmp)      ;_ add VVA 20.03.2009
                   )
		 )                        ;_ add VVA 20.03.2009
	 )                                ;_ add VVA 20.03.2009
       )                                  ;_ add VVA 20.03.2009


(foreach itm SS-LTYPE
  (bg:change-prop itm "Linetype" BLTYPE)
  )
(foreach itm SS-COLOR
  (bg:change-prop itm "Color" BCOLOR)
  )
(foreach itm SS-LWEIGHT
  (bg:change-prop itm "LineWeight" BLWEIGHT)
  )
(foreach itm SS-LAYER
  (bg:change-prop itm "Layer" BLAYER)
  )
    )
    (_bump (strcat "Block " BBLOCK  " not Explodable. Ignored. "))
    )
  (if (> (sslength SS-ATTR) 0)(command "_draworder" SS-ATTR "" "_f"))
  (setq SS-LAYER nil SS-COLOR nil SS-LTYPE nil SS-LWEIGHT NIL SS-ATTR NIL)
)

(Defun LASTENT (/ E0 EN)
 ;-----------------------------------------------------
 ; Find True last entity
 ;-----------------------------------------------------
  
      (Setq E0 (EntLast))
      (While (Setq EN (EntNext E0))
         (Setq E0 EN)
      )
      E0
   )

(Defun bg:EXPLODABLE (BNAME / B expld)
      (setq BLOCKS (vla-get-blocks 
                     (vla-get-ActiveDocument (vlax-get-acad-object)))
       )
      
      (vlax-for B BLOCKS (if (and (= :vlax-false (vla-get-islayout B))
                                  (= (strcase (vla-get-name B)) (strcase BNAME))
                                  )
                           (if (vlax-property-available-p B "explodable")  ;;; VVA Correct to 2004 AutoCAD
                             (setq expld (= :vlax-true (vla-get-explodable B)))  
                             (setq expld t)
                             )
                           )
           )
       expld
    )
  
(defun bg:change-prop ( obj prop value)
  (if (= (type obj) 'ENAME)
      (setq obj (vlax-ename->vla-object obj))
    )
   (if (and
         (vlax-write-enabled-p obj)
         (vlax-property-available-p obj prop)
         )
     (vl-catch-all-apply 'vlax-put-property (list obj prop value))
     )
  )