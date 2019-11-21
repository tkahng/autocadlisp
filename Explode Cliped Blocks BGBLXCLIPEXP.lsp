;;; ===================================================================================;;
;;;  BGTOOLS (Background tools) v.2.31 Copyright © 2009-2014                           ;;
;;;  Idea conception by: Vladimir Svet (VOVA from dwg.ru)                              ;;
;;;  Programmation by: Vladimir Azarko (VVA from dwg.ru)                               ;;
;;;  special thanks: Пётр Лоскутов (Alaspher),LEE MAC,theswamp.org,dwg.ru,cadtutor.net ;;
;;;                                                                                    ;;
;;; Published                                                                          ;;
;;;  http://forum.dwg.ru/showthread.php?p=735810                                       ;;
;;;                                                                                    ;;
;;; ===================================================================================;;
;;;                                                                                    ;;
;;;  THIS PROGRAM AND PARTS OF IT MAY REPRODUCED BY ANY METHOD ON ANY                  ;;
;;;  MEDIUM FOR ANY REASON. YOU CAN USE OR MODIFY THIS PROGRAM OR                      ;;
;;;  PARTS OF IT ABSOLUTELY FREE.                                                      ;;
;;;                                                                                    ;;
;;;  THIS PROGRAM PROVIDES 'AS IS' WITH ALL FAULTS AND SPECIFICALLY                    ;;
;;;  DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR FITNESS                      ;;
;;;  FOR A PARTICULAR USE.                                                             ;;
;;;                                                                                    ;;
;;; ===================================================================================;;
;;;                                                                                                             ;;
;;;=============================================================================================================;;
;;;               Command(s) to call EN                                                                         ;;
;;;=============================================================================================================;;
;;;                                                                                                             ;;
;;;BGLAYDEL - delete frozen and off layers with objects
;;;BG1BLEXP - Explode blocks consisting of one element (it is possible other block)
;;;BGBLEXP  - Explode the block and all blocks entering into it, transforming visible attributes in the text
;;;BGBLEXP1 - Explode blocks of 1st level, transforming visible attributes in the text
;;;BGBLDYNEXP1 - Explode dynamic blocks of 1st level, transforming visible attributes in the text
;;;BGBLEXP1NOATT -Explode blocks of 1st level which do not have visible attributes 
;;;BGBLXCLIP - show xclip blocks
;;;BGBLDYN2A - Convert Dynamic Blocks to Anonymous blocks
;;;BGBLDYN2S - Convert Dynamic Blocks to Static blocks
;;;BGBLALLDYN2A - Conver ALL Dynamic Blocks to Anonymous
;;;BGBLALLDYN2S - Conver ALL Dynamic Blocks to Static
;;;BGBLXCLIPEXP - explode clipped block
;;;BGCB - copy a single block reference in the working drawing
;;;BGRB - rename a single block reference in the working drawing
;;;M2U - Convert MINSERT to Unnamed block
;;;M2B - Convert MINSERT to block
;;;U2B - Convert UNNAMED to block
;;;U2BM - Convert UNNAMED to block multiple selection
;;;UX - Explode UNNAMED block
;;;=======================================
;;;BGRGB2ACI - Changes color from RGB to the corresponding Index Color (ACI)
;;;BGAll2RGB - Converts the ACI colors of all entities to the RGB TrueColor equivalent
;;;BGCOLOR - Set a selected color to all objects (WITHOUT XREF)
;;;BGCOLORXREF - Changes color selected XREF ( ONLY ON A CURRENT SESSION )
;;;BGResetXRef -Resets properties of all layers dependent on the selected XREF(s) to those set in the source drawing file(s).
;;;BGBLCC - Changes color of the chosen blocks
;;;BGENCC - Changes color of the chosen element of the block
;;;BGCATT - Changes color of attributes of the chosen blocks
;;;BGCBL2 - Changes color in blocks 2 and more levels (the nested blocks)
;;;BGBLFIX - fix (normalize) blocks
;;;BGCFT   - Convert field to text
;;;BGCFTSEL - Convert field to text in selected objects
;;;BGHATCHDEL - Remove hatch
;;;                                                                                                             ;;
;;;=============================================================================================================;;
;;;               Command(s) to call RUS                                                                        ;;
;;;=============================================================================================================;;
;;;                                                                                                             ;;
;;;BGLAYDEL - Удаление выключенных и замороженных слоев с примитивами
;;;BG1BLEXP - Взрывает блоки, состоящие из одного примитива (возможно другого блока)
;;;BGBLEXP  - Взрывает блок и все блоки, входящие в него, превращая видимые атрибуты в текст
;;;BGBLEXP1 - Взрывает блоки 1-го уровня, превращая видимые атрибуты в текст
;;;BGBLDYNEXP1 - Взрывает динамические блоки 1-го уровня, превращая видимые атрибуты в текст
;;;BGBLEXP1NOATT - Взрывает блоки 1-го уровня, у которых нет видимых атрибутов
;;;BGBLXCLIP - Показывает подрезанные блоки
;;;BGBLDYN2A - Преобразовывает динамические блоки в анонимные
;;;BGBLDYN2S - Преобразовывает динамические блоки в статические
;;;BGBLALLDYN2A - Преобразовывает ВСЕ динамические блоки в анонимные
;;;BGBLALLDYN2S - Преобразовывает ВСЕ динамические блоки в статические
;;;BGBLXCLIPEXP - взорвать подрезанный блок
;;;BGCB - копирует одно из вхождений блока в блок с новым именем
;;;BGRB - переименовывает одно из вхождений блока в блок с новым именем
;;;M2U - Преобразовать MINSERT в Unnamed блок
;;;M2B - Преобразовать MINSERT в блок
;;;U2B - Преобразовать UNNAMED в блок
;;;U2BM - Преобразовать UNNAMED в блок множественный выбор
;;;UX - Взорвать UNNAMED блок
;;;=======================================
;;;BGRGB2ACI - Изменяет цвет с RGB на ACI
;;;BGAll2RGB - Изменяет ACI цвет в его RGB эквивалент
;;;BGCOLOR - Задает выбранный цвет всем объектам
;;;BGCOLORXREF - Меняет цвет выбранных внешних сылок (только в текушем сеансе)
;;;BGResetXRef -Сброс настроек слоев выбранных внешних ссылок на установленные в 
;;;исходном файле
;;;BGBLCC - Изменяет цвет выбранных блоков
;;;BGENCC - Изменяет цвет выбранного элемента блока
;;;BGCATT - Изменяет цвет атрибутов выбранных блоков
;;;BGCBL2 - Изменяет цвет в блоках 2 и более уровня (вложенных блоках)
;;;BGBLFIX - Нормализация блоков
;;;BGCFT   - Конвертирует все поля в обычный текст
;;;BGCFTSEL - конвертирование полей в обычный текст в выбранных примитивах
;;;BGHATCHDEL - Удаление штриховки


;;;=============================================================================================================;;
;;;                                                                                                             ;;
;;;                                HISTORY                                                                      ;;
;;;=============================================================================================================;;
;;;               EN                                                                                            ;;
;;;=============================================================================================================;;
;;;                                                                                                             ;;
;;; 1.0 - fixed burst - the inheritance of the line type byblock, displaying hidden attributes, and inheritance type of line unit
;;; Added processing Burst LineWeight
;;; 1.1 unit change in color in the presence of attributes (cm;;; Change VVA 12.09.2008)
;;; ...
;;: 1.4 Handling multiline styles
;;; 1.11 ChangeAllObjectsColor - Handling size, leader and mtext with an explicit color.
;;; BGBLEXP - processing appearances dynamic blocks (if the visibility attributes are included)
;;; BG1BLEXP - Obabatyvayutsya units consisting of one element and
;;; - Name of the block A$C*
;;; - Or any other name and this one element of another block
;;; 1.12 - Do not remove the blocks, located on the off / frozen layer
;;; And having at least one primitive layer turned on, other than "0"
;;; 2.0 - BGBLEXP - Fixed a bug with the list
;;; 2.1 - processing blocks external links ChangeXrefAllObjectsColor
;;; 2.2 - Add command  BGCFT
;;; 2.3 - add BGCOLORXREF and BGRGB2ACI
;;; 2.4 - Processing of multi-attribute
;;; 2.5 - Added bg:FieldCode ChangeAllObjectsColor ChangeXrefAllObjectsColor - processing of Unicode characters
;;; 2.6 - Adding a BGBLDYN2A BGBLDYN2S
;;; 2.7 - Added handling of pre-selection
;;; 2.8 - Error handling and visibility attributes
;;; 2.9 - Added BGBLDYNEXP1
;;; 2.10 - Convert dynamic blocks bg:DynToStatic. Saving and restoring the values of attributes
;;; 2.11 - Changes in the bg:get-all-atts (processing UNICOD characters in attribute values)
;;;        BGHATCHDEL
;;;        ChangeAllObjectsColor - change table text and grid color
;;; 2.12 - Counted lineweight to the attributes
;;; 2.13 - Add BG:GET-TEXTSTRING functions
;;; 2.14  - Add BGAll2RGB and Change ClearFieldInAllObjects look http://forum.dwg.ru/showthread.php?t=76285  post #6
;;; 2.15  - minor change in BGAll2RGB. Add BGCB and BGRB
;;;;2.16  - BGCBL2 now work with ACI and RGB color
;;;;2.17  - BGBLCC now work with ACI and RGB color and other minor change
;;; 2.18  - add command BGBLXCLIPEXP
;;; 2.19  - BGBLFIX add lock layer
;;; 2.20  - BGBLFIX fix bugs
;;; 2.21  - BGBLENCC fix bugs.
;;; 2.22  - Add Scale Uniformly and Allow Expoding options
;;; 2.23  - Fix BGCB BGRB command
;;; 2.24  - Edit UX command
;;; 2.25  - Fix BGBLCC и BGCBL2 command. Add BGCFTSEL
;;; 2.26  - Fix BGCFT command. Add BGBLALLDYN2A and BGBLALLDYN2S
;;; 2.27  - in command U2B, UX added processing blocks named *E
;;; 2.28    Fixed minor bugs in BGCFT command and function bg:DynToStatic
;;; 2.29    Fixed minor bugs in function ClearFieldInThisObject
;;; 2.30    Fixed minor bugs in function ClearFieldInThisObject
;;;         http://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/vla-get-textstring-u-symbols/td-p/4365165
;;; 2.31    BGCFT clear field in dimention now
;;;         (http://forum.dwg.ru/showthread.php?p=1335110#post1335110)

;;;=============================================================================================================;;
;;;               RUS                                                                                           ;;
;;;=============================================================================================================;;
;;;	1.0 - исправлены ошибки burst - при наследовании типа линии поблоку, показ скрытых атрибутов и наследования типа линии блока
;;;	                                если в блок примитив входил не на 0 слое, тип линии поблоку.
;;;	      добавлена обработка Burst LineWeight
;;;	1.1  изменение цвета блока при наличии атрибутов (см ;;; Change VVA 12.09.2008)
;;;	...
;;;	1.4  Обработка стилей мультилиний
;;;	1.11 ChangeAllObjectsColor - Обработка размеров, leader и mtext с явным заданием цвета.
;;;	     BGBLEXP - обработка видимостей динамических блоков (если в видимости включены атрибуты)
;;;	     BG1BLEXP - Обабатываются блоки, состоящие из одного элемента и
;;;	     - имя блока A$C*
;;;	     - или любое другое имя и этот один элемент другой блок
;;;	1.12 - Не удаляет блоки, расположенные на выключенном/ замороженном слое
;;;	       и имеющие хотя бы 1 примитив на включенном слое, отличном от "0"
;;;	2.0  - BGBLEXP - исправлена ошибка со списком
;;;	2.1  - обработка блоков внешних ссылок ChangeXrefAllObjectsColor
;;;	2.2  - команда BGCFT
;;;	2.3  - add BGCOLORXREF and BGRGB2ACI
;;;	2.4  - Обработка многострочных атрибутов
;;;     2.5  - Добавленна bg:FieldCode ChangeAllObjectsColor ChangeXrefAllObjectsColor - обработка Unicode символов
;;;     2.6  - Добавленны команды BGBLDYN2A BGBLDYN2S
;;;     2.7  - Добавлена обработка предварительного выбора
;;;     2.8  - Ошибки с обработкой атрибутов и visibility
;;;     2.9  - Добавлена BGBLDYNEXP1
;;;     2.10  - Конвертация динамических блоков  bg:DynToStatic. Сохранение и восстановление значений атрибутов
;;;     2.11 - Изменения в bg:get-all-atts (обработка UNICOD символов в значениях атрибутов)
;;;            BGHATCHDEL
;;;            ChangeAllObjectsColor - change table text and grid color
;;;    2.12  - Учтен lineweight для атрибутов
;;;    2.13  - Добавлена функция BG:GET-TEXTSTRING (как замена vla-get-textstring. Т.к. неверно возвращает строку при наличии юникод символов)
;;;    2.14  - Добавлена команда BGAll2RGB Изменена ClearFieldInAllObjects см http://forum.dwg.ru/showthread.php?t=76285  post #6
;;;    2.15  - Небольшие изменения в BGAll2RGB. добавлены BGCB и BGRB
;;;;   2.16  - BGCBL2 теперь работает с RGB и ACI цветами
;;;;   2.17  - BGBLCC  теперь работает с RGB и ACI цветами + мелкие изменения
;;;;   2.18  - Учтено предложение http://forum.dwg.ru/showpost.php?p=949149&postcount=102
;;;;           Добавлена команда BGBLXCLIPEXP
;;;;   2.19  - Команда BGBLFIX. Добавлена опция игнорирования слоев
;;;;   2.20  - Команда BGBLFIX. Испрален баг с атрибутами
;;;    2.22  - Добавлены опции Одинаковые масштабы и Разрешить расчленение
;;;    2.23  - Исправлены ошибки в командах BGRB и BGCGB
;;;    2.24  - Отредактирована команда UX (вложенность анонимных блоков)
;;;    2.25  - http://forum.dwg.ru/showthread.php?p=1024096#post1024096
;;;    2.25  - Fix BGBLCC и BGCBL2 command http://forum.dwg.ru/showthread.php?p=1088187#post1088187
;;;            Add BGCFTSEL http://forum.dwg.ru/showthread.php?t=20190&page=2
;;;    2.26  - BGCFT - добавлена обработка формата текста в таблице. Добавлены команды BGBLALLDYN2A и BGBLALLDYN2S
;;;    2.27  - в коммандах U2B ,UX добавлена обработка блоков с именем *E
;;;    2.28  - BGCFT - исправлена ошибка + изменения в function bg:DynToStatic
;;;    2.29  - Исправлен недочет ClearFieldInThisObject
;;;    2.30    Fixed minor bugs in function ClearFieldInThisObject
;;;         http://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/vla-get-textstring-u-symbols/td-p/4365165
;;;    2.31    BGCFT добавлена очистка полей в размерах
;;;            (http://forum.dwg.ru/showthread.php?p=1335110#post1335110)

(vl-load-com)

(defun bg:purge-layer-filter ()
;;;Purge layer filter
;;; Удаление фиольтров слоев  
  (mapcar '(lambda (dictionary)
             (vl-catch-all-apply
               '(lambda ()
                  (vla-remove
                    (vla-getextensiondictionary
                      (vla-get-layers
                        (vla-get-activedocument
                          (vlax-get-acad-object)
                        ) ;_ end of vla-Get-ActiveDocument
                      ) ;_ end of vla-Get-Layers
                    ) ;_ end of vla-GetExtensionDictionary
                    dictionary
                  ) ;_ end of vla-Remove
                ) ;_ end of lambda
             ) ;_ end of vl-Catch-All-Apply
           ) ;_ end of lambda
          '("ACLYDICTIONARY" "ACAD_LAYERFILTERS" "ACAD_LAYERSTATES")
  ) ;_ end of mapcar
) ;_ end of defun
(defun bg:purge-groups ( / grpList index grp)
;; Ф-ция bg:purge-groups
;; Удаляет описание всех групп
;; Аргумент [Тип]:
;;   НЕТ
;; Возвращает: Nil
;;;;;;		  (setq grpList (dictsearch (namedobjdict) "ACAD_GROUP"))
;;;;;;		  (setq index 1)
;;;;;;		  (while (setq grp (nth index grplist))
;;;;;;		    (if  (= (car grp) 3)
;;;;;;		      (entdel (cdr (nth (+ index 1) grplist)))
;;;;;;		    )
;;;;;;		    (setq index (+ 1 index))
;;;;;;		  )
;;;;;;		  (princ)
  (vlax-for grp (vla-item (vla-get-Dictionaries (vla-get-activedocument (vlax-get-acad-object))) "ACAD_GROUP")
     (vl-catch-all-apply  'vla-delete (list grp))
   )
 )
(defun bg:purge-AllPageSetups (/ pc)
  ;;;Удаление сохраненных page setups
  (vlax-for pc (vla-get-plotconfigurations (vla-get-activedocument (vlax-get-acad-object)))
    (vla-delete pc)
  )
)

(defun bg:purge-apps ( / Blk Obj appid)
;;;Удаление зарегестрированных приложений
(vlax-for Blk (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
    (if (= (vla-get-isxref Blk) :vlax-false)
      (progn
        (grtext -1
                (strcat "Inspecting objects: "
                        (vla-get-name Blk)
                ) ;_ end of strcat
        ) ;_ end of grtext

        (vlax-for Obj Blk (bg:del-XDATA-from-ename Obj nil))
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of vlax-for
(vlax-for appid (vla-get-registeredapplications
		    (vla-get-activedocument
		      (vlax-get-acad-object)
		    )
		  )
    (vl-catch-all-apply 'vla-delete (list appid))
  )  
  )
(defun bg:purge-ACADVBA ()
  (dictremove (namedobjdict) "ACAD_VBA")
  (princ)
)
(defun bg:purge-DATALINK ()
  (dictremove (namedobjdict) "ACAD_DATALINK")
  (princ)
)

(defun bg:purge-all ()
  (grtext -1 "Audit ... ")
  (vla-auditinfo (vla-get-activedocument (vlax-get-acad-object)) :vlax-true)  
  (grtext -1 "purge layer filter")
  (bg:purge-layer-filter)
  (grtext -1 "purge groups")
  (bg:purge-groups)
  (grtext -1 "purge Page Setups")
  (bg:purge-AllPageSetups)
  (bg:purge-ACADVBA)
  (bg:purge-DATALINK)
  (grtext -1 "purge Apps")
  (bg:purge-apps)
  ;;; http://www.theswamp.org/index.php?topic=42110.0
  ;;; (dictremove (namedobjdict) "ACAD_DGNLINESTYLECOMP")
  (repeat 3 (vla-purgeall (vla-get-activedocument (vlax-get-acad-object))))
  (command "_.Regenall")
  )
(defun bg:progress-init (msg maxlen)
  ;;; msg - сообщение или пустая строка
  ;;; maxlen - максимальное количество
  (or *BG:PROGRESS:OM* (setq *BG:PROGRESS:OM* (getvar "MODEMACRO")))
  (setq *BG:PROGRESS:MSG* (vl-princ-to-string msg))
  (setq *BG:PROGRESS:MAXLEN* maxlen)
  (setq *BG:PROGRESS:LPS* '-1)(princ)
  )
(defun bg:progress ( currvalue / persent str1 count)
  (if *BG:PROGRESS:MAXLEN*
    (progn
  (setq persent (fix (/ currvalue 0.01 *BG:PROGRESS:MAXLEN*)))
  ;;;Каждые 5 %
  (setq count (fix(* persent 0.2)))
  (setq str1 "")
  (if (/= count *BG:PROGRESS:LPS*)
    (progn
      ;;(setq str1 "")
      (repeat persent (setq str1 (strcat str1 "|")))
      )
    )
       ;;; currvalue - текущее значение
      (setvar "MODEMACRO"
              (strcat (vl-princ-to-string *BG:PROGRESS:MSG*)
                      " "
                      (itoa persent)
                      " % "
                      str1
                      )
              )
      (setq *BG:PROGRESS:LPS* persent)
  )
    )
  )
     
(defun bg:progress-clear ()
  (setq *BG:PROGRESS:MSG* nil)
  (setq *BG:PROGRESS:MAXLEN* nil)
  (setq *BG:PROGRESS:LPS* nil)
  (setvar "MODEMACRO" (vl-princ-to-string *BG:PROGRESS:OM*))
  ;;;(vla-regen (vla-get-activedocument (vlax-get-acad-object)) acactiveviewport)
  (princ)
  )
;;;; Функция выводит диалоговое окно Да-Нет
;;;;title - заголовок
;;;;message - сообщение
;;;; Возвращает t - Да
;;;;            nil - Нет
;;; Пример (bg:MSG-YES-NO "Привет" "Пойдем в кино?")
;;;(defun bg:msg-yes-no ( title message / usri1 ret)
;;;(setq usri1 (getvar "USERI1"))
;;;(SETVAR "USERI1" 0)
;;;(command "_vbastmt"
;;;(strcat "ThisDrawing.SetVariable \"USERI1\", "
;;;"MsgBox \(\""
;;;message
;;;"\","
;;;(itoa vlax-VBYesNo)
;;;",\""
;;;title
;;;"\"\)"
;;;)
;;;)
;;;(setq ret (= (getvar "USERI1") 6))
;;;(SETVAR "USERI1" usri1)
;;;ret	 
;;;)
(defun mip_MTEXT_Unformat ( Mtext / text Str )
  ;;;http://www.caduser.ru/forum/index.php?PAGE_NAME=read&FID=44&TID=20992
  (setq Text "")
;;;  (if (wcmatch (strcase Mtext) "\\PI-#*,\\PT*") ;;_список
;;;    (setq Mtext (substr Mtext (+ 2 (vl-string-search ";" Mtext)))))
   (while (/= Mtext "")
        (cond
          ((wcmatch (strcase (setq Str (substr Mtext 1 2))) "\\[\\{}]")
            (setq Mtext (substr Mtext 3) Text   (strcat Text Str)))
          ((wcmatch (substr Mtext 1 1) "[{}]")(setq Mtext (substr Mtext 2)))
          ((wcmatch (strcase (setq Str (substr Mtext 1 2))) "\\[LO`~]")
	   (setq Mtext (substr Mtext 3)))
          ((wcmatch (strcase (substr Mtext 1 8)) "\\FSYMBOL") ;;;Add VVA remove Symbol
            (setq Mtext (substr Mtext (+ 2 (cond ((vl-string-search "}" Mtext))((vl-string-search ";" Mtext)))))))
          ((wcmatch (strcase (substr Mtext 1 2)) "\\[ACFHQTW]")
            (setq Mtext (substr Mtext (+ 2 (vl-string-search ";" Mtext)))))
	  ((wcmatch (strcase Mtext) "\\PI-#*,\\PT*")           ;;;VVA 2011-01-20
	   (setq Mtext (substr Mtext (+ 2 (vl-string-search ";" Mtext)))))
	  ((wcmatch (strcase (substr mtext 1 4)) "\\PQ[CRJD],\\PTZ")  ;;;Add by KPblC
	   (setq mtext (substr mtext (+ 2 (vl-string-search ";" mtext))))
	   )
          ((wcmatch (strcase (substr mtext 1 3)) "\\PX")  ;;;абзац и междустрочный интервал \\PX[QITSBA]
	   (setq mtext (substr mtext (+ 2 (vl-string-search ";" mtext))))
	   )
          ((wcmatch (strcase (substr Mtext 1 2)) "\\P")
            (if (or
		   (zerop (strlen Text))
		   (= " " (substr Text (strlen Text)))
		   (= " " (substr Mtext 3 1)))
               (setq Mtext (substr Mtext 3))
               (setq Mtext (substr Mtext 3) Text (strcat Text " "))))
	  ((wcmatch (strcase (substr Mtext 1 2)) "\\S")
            (setq Str   (substr Mtext 3 (- (vl-string-search ";" Mtext) 2))
                  Text  (strcat Text (vl-string-translate "#^\\" "/^\\" Str))
                  Mtext (substr Mtext (+ 4 (strlen Str)))))
          
	  (t (setq Text (strcat Text (substr Mtext 1 1)) Mtext (substr Mtext 2)))))
  Text)

(defun bg:msg-Popup ( title message flags / WScript ret )
  ;;; title - text to displayed in title bar
  ;;; message - message
  ;;; flags - integer icon+ button See http://msdn2.microsoft.com/en-us/library/x83z1d9f.aspx
  ;;; intButton = object.Popup(strText,[nSecondsToWait],[strTitle],[nType])
  ;;; Use (bg:msg-Popup "Test" "2+2=4\nRight?" (+ 4 32 4096))
  
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; Коды nType Сумма Кнопки + Иконки

;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; Кнопки Button
;;;0 - Show OK button.
;;;1 - Show OK and Cancel buttons.
;;;2 - Show Abort, Retry, and Ignore buttons.
;;;3 - Show Yes, No, and Cancel buttons.
;;;4 - Show Yes and No buttons.
;;;5 - Show Retry and Cancel buttons.

;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; Иконки Icon Types
;;;16 - Show "Stop Mark" icon.
;;;32 - Show "Question Mark" icon.
;;;48 - Show "Exclamation Mark" icon.
;;;64 - Show "Information Mark" icon.

;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Other
  
;;;256	The second button is the default button.
;;;512	The third button is the default button.
;;;4096	The message box is a system modal message box and appears in a topmost window.
;;;524288	The text is right-justified.
;;;1048576	The message and content text display in right-to-left reading order.
  
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Return - intButton Возвращаемое значение

;;;1 - OK button
;;;2 - Cancel button
;;;3 - Abort button
;;;4 - Retry button
;;;5 - Ignore button
;;;6 - Yes button
;;;7 - No button

;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  (vl-catch-all-apply
    (function
      (lambda ()
        (setq WScript (vlax-create-object "WScript.Shell"))
        (setq ret (vlax-invoke-method WScript "popup" message "0" title flags))
      )
    )
  )
  (if WScript (vlax-release-object WScript))
  ret
)
(defun bg:msg-yes-no ( title message)
  ;;; title - text to displayed in title bar
  ;;; message - message
  ;;; Return T - Yes button NIl - No button
  (= (bg:msg-Popup title message (+ 4 48)) 6)
  )

  ;|
* Ф-ция str-str-lst
* Сервисная ф-ция извлечения из строки данных, разделенных
* каким либо символом или строкой символов
* Возвращает список строк
* Аргументы [Type]:
  str - строка для разбора [STRING]
  pat - разделитель [STRING]
*  Пример запуска
  (setq str "мы;изучаем;рекурсии" pat ";")
  (setq str "мы — изучаем — рекурсии" pat " — ")
  (str-str-lst str pat)
* Читать подробнее http://www.caduser.ru/cgi-bin/f1/board.cgi?t=25113OT
|;
(defun str-str-lst (str pat / i)
  (cond ((= str "") nil)
        ((setq i (vl-string-search pat str))
         (cons (substr str 1 i)
               (str-str-lst (substr str (+ (strlen pat) 1 i)) pat)
         ) ;_  cons
        )
        (t (list str))
  ) ;_  cond
) ;_  defun

(defun bg:layer-status-restore ()
    (foreach item *BG_LAYER_LST*
      (if (not (vlax-erased-p (car item)))
        (vl-catch-all-apply
          '(lambda ()
             (vla-put-lock (car item) (cdr (assoc "lock" (cdr item))))
             (vla-put-freeze (car item) (cdr (assoc "freeze" (cdr item))))
             ) ;_ end of lambda
          ) ;_ end of vl-catch-all-apply
        ) ;_ end of if
      ) ;_ end of foreach
    (setq *BG_LAYER_LST* nil)
    ) ;_ end of defun

  (defun bg:layer-status-save ()
    (setq *BG_LAYER_LST* nil)
    (vlax-for item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
      (setq *BG_LAYER_LST* (cons (list item
                                  (cons "freeze" (vla-get-freeze item))
                                  (cons "lock" (vla-get-lock item))
                                  ) ;_ end of cons
                            *BG_LAYER_LST*
                            ) ;_ end of cons
            ) ;_ end of setq
      (vla-put-lock item :vlax-false)
      (if (= (vla-get-freeze item) :vlax-true)
      (vl-catch-all-apply '(lambda () (vla-put-freeze item :vlax-false))))
      ) ;_ end of vlax-for
    ) ;_ end of defun
(defun bg:get-TextString (ent / elst str)
  ;;; ent -entity name
  ;;; return - text string
    (setq elst (entget ent))
  (if (vlax-property-available-p (vlax-ename->vla-object ent) 'Textstring)
    (cond ((= (cdr(assoc 0 elst)) "MULTILEADER")
           (setq str (cdr(assoc 304 elst)))
           )
          ((and ;;; MTEXT ATTRIB ADD VVA 2011-20-27
             (member (cdr(assoc 0 elst)) '("ATTRIB"))
             (member '(101 . "Embedded Object") elst)
             )
           (setq str (apply 'strcat (append (bg:massoc 3 elst)(bg:massoc 1 (member '(101 . "Embedded Object") elst)))))
           )
          
          ((member (cdr(assoc 0 elst)) '("TEXT" "MTEXT" "ATTRIB"))
           (setq str (apply 'strcat (append (bg:massoc 3 elst)(bg:massoc 1 elst))))
           )
          (t (setq str (vla-get-TextString (vlax-ename->vla-object ent))))
    )
    )
str
  )
;|  ! ***************************************************************************
;; !                           bg:massoc
;; ! ***************************************************************************
;; ! Function : Utility function to get multiple group code
                Возвращает все вхождения ключа в списке
;; ! Argument : 'key'     - The DXF code to check / DXF код
;; !            'alist' -    The List to check  / Список
;; ! Returns  : The value of the DXF dotted pair, if it exists else returns nil
                Значение точечной пары, если есть или nil
;; ! ****************************************************************************|;
;;; Utility function to get multiple group code CDRs
(defun bg:massoc (key alist)
  ;;;lib:massoc mip_lib.lsp
  (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (= key (car x)))) alist)))
(defun bg:FieldCode (ent / foo elst xdict dict field str)
  ;; credits gile gc:FieldCode
  (defun foo (field str / pos fldID objID)
    (setq pos 0)
    (if (setq pos (vl-string-search "\\_FldIdx " str pos))
      (while (setq pos (vl-string-search "\\_FldIdx " str pos))
        (setq fldId (entget (cdr (assoc 360 field)))
              field (vl-remove (assoc 360 field) field)
              str   (strcat
                      (substr str 1 pos)
                      (if (setq objID (cdr (assoc 331 fldId)))
                        (vl-string-subst
                          (strcat "ObjId " (itoa (gc:EnameToObjectId objID)))
                          "ObjIdx"
                          (cdr (assoc 2 fldId))
                        )
                        (foo fldId (cdr (assoc 2 fldId)))
                      )
                      (substr str (1+ (vl-string-search ">%" str pos)))
                    )
        )
      )
      str
    )
  )
;; gc:EnameToObjectId (gile)
;; Returns the ObjectID from an ename
;;
;; Argument : an ename

(defun gc:EnameToObjectId (ename)
  ;; credits gile
  ((lambda (str)
     (hex2dec
       (substr (vl-string-right-trim ">" str) (+ 3 (vl-string-search ":" str)))
     )
   )
    (vl-princ-to-string ename)
  )
)
;;============================================================;;
;; hex2dec (gile)
;; convert an hexadecimal into a decimal (int)
;;
;; Argument : un hexadedimal (string)

(defun hex2dec (s / r l n)
  (setq	r 0 l (vl-string->list (strcase s)))
  (while (setq n (car l))
    (setq l (cdr l)
          r (+ (* r 16) (- n (if (<= n 57) 48 55)))
    )
  )
)
 ;;--------------------------------------------------------;;
  
  (setq elst (entget ent))
  (if (vlax-property-available-p (vlax-ename->vla-object ent) 'Textstring)
    (cond ((= (cdr(assoc 0 elst)) "MULTILEADER")
           (setq str (cdr(assoc 304 elst)))
           )
          ((and ;;; MTEXT ATTRIB ADD VVA 2011-20-27
             (member (cdr(assoc 0 elst)) '("ATTRIB"))
             (member '(101 . "Embedded Object") elst)
             )
           (setq str (apply 'strcat (append (bg:massoc 3 elst)(bg:massoc 1 (member '(101 . "Embedded Object") elst)))))
           )
          
          ((member (cdr(assoc 0 elst)) '("TEXT" "MTEXT" "ATTRIB"))
           (setq str (apply 'strcat (append (bg:massoc 3 elst)(bg:massoc 1 elst))))
           )
          (t (setq str (vla-get-TextString (vlax-ename->vla-object ent))))
    )
    )
  (if (and
	(member (cdr (assoc 0 elst)) '("ATTRIB" "MTEXT" "TEXT" "MULTILEADER"))
	(setq xdict (cdr (assoc 360 elst)))
	(setq dict (dictsearch xdict "ACAD_FIELD"))
	(setq field (dictsearch (cdr (assoc -1 dict)) "TEXT"))
      )
    (setq str (foo field (cdr (assoc 2 field))))
  )
    str
)
(defun ChangeAllObjectsColor (Doc Color  / txtstr tmp txt count _mtext-color-clean)
  
  (defun _mtext-color-clean ( txtstr / tmp )
    ;;;(setq txtstr(vla-gettext obj 1 1))
	    (setq tmp 0)
	    (while (setq tmp (VL-STRING-SEARCH "\\C" txtstr tmp))
	      (setq txtstr
	      (vl-string-subst
		(strcat (substr txtstr (1+ tmp) 2)(itoa Color) ";")
		(substr txtstr (1+ tmp) (- (1+ (VL-STRING-SEARCH ";" txtstr tmp)) tmp))
		txtstr
		tmp)
		    )
	      (setq tmp (+ tmp 3))
	      )
    txtstr
  )
  (vlax-for Blk	(vla-get-Blocks Doc)
    (if (= (vla-get-IsXref Blk) :vlax-false)
      (progn
	(setq count 0 txt (strcat "Changed " (vla-get-name Blk)))
	(grtext -1 txt)
      (if (not (wcmatch (vla-get-name Blk) "`*T*,`*D*")) ;_exclude table  dimension
      (vlax-for	Obj Blk
	(setq count (1+ count))
	(if (zerop(rem count 10))(grtext -1 (strcat txt " : " (itoa count))))
	(if (and (vlax-write-enabled-p Obj)
		 (vlax-property-available-p Obj 'Color)
	    ) ;_ end of and
	  (vla-put-Color Obj Color)
	) ;_ end of if
	(if (and (vlax-write-enabled-p Obj)
		 (vlax-property-available-p Obj 'TextString)
	    ) ;_ end of and
	  (progn
;;; >>>--------- comment VVA 2011-04-28            
;;;	    (setq txtstr
;;;		   (if (vlax-method-applicable-p Obj 'FieldCode)
;;;		       (vla-FieldCode Obj)
;;;		       (vlax-get-property Obj 'TextString))
;;;		  )
;;; <<< --------- comment VVA 2011-04-28            

;;; >>>--------- ADD VVA 2011-04-28                                    
;;;            (setq txtstr (bg:FieldCode (vlax-vla-object->ename Obj)))
;;; <<<--------- ADD VVA 2011-04-28
;;;	    (setq tmp 0)
;;;	    (while (setq tmp (VL-STRING-SEARCH "\\C" txtstr tmp))
;;;	      (setq txtstr
;;;	      (vl-string-subst
;;;		(strcat (substr txtstr (1+ tmp) 2)(itoa Color) ";")
;;;		(substr txtstr (1+ tmp) (- (1+ (VL-STRING-SEARCH ";" txtstr tmp)) tmp))
;;;		txtstr
;;;		tmp)
;;;		    )
;;;	      (setq tmp (+ tmp 3))
;;;	      )
	    (vla-put-Textstring Obj (_mtext-color-clean (bg:FieldCode (vlax-vla-object->ename Obj)))) ;;;CH VVA 15.09.2011
	    )
	) ;_ end of if
	(if (and (vlax-write-enabled-p Obj)
		 (= (vla-get-ObjectName obj) "AcDbBlockReference")
		 (= (vla-get-HasAttributes obj) :vlax-true)
	    ) ;_ end of and
	  (foreach att (vlax-safearray->list
			 (vlax-variant-value (vla-GetAttributes obj))
		       ) ;_ end of vlax-safearray->list
	    (if	(and (vlax-write-enabled-p att)
		     (vlax-property-available-p att 'Color)
		) ;_ end of and
	      (vla-put-Color att Color)
	    ) ;_ end of if
	  ) ;_ end of foreach
	) ;_ end of if
        (if (and (vlax-write-enabled-p Obj)
		  (wcmatch (vla-get-Objectname Obj)  "*Dimension*,AcDb*Leader")
	     ) ;_ end of and
	   (progn
	     (vl-catch-all-apply 'vla-put-ExtensionLineColor (list Obj Color))
	     (vl-catch-all-apply 'vla-put-TextColor (list Obj Color))
	     (vl-catch-all-apply 'vla-put-DimensionLineColor (list Obj Color))
	     (if (vlax-property-available-p Obj 'LeaderLineColor)
	       (progn
		 (setq tmp (vla-getinterfaceobject(vlax-get-acad-object)(strcat "AutoCAD.AcCmColor."
		(substr (getvar "ACADVER") 1 2))))
		 (vla-put-colorindex  tmp  Color)
		 (vl-catch-all-apply 'vla-put-LeaderLineColor (list Obj tmp))
		 )
	       )
	   ) ;_ end of progn
	 ) ;_ end of if
        (if (and (vlax-write-enabled-p Obj)
		  (wcmatch (vla-get-Objectname Obj)  "AcDbTable")
	     ) ;_ end of and
	   (progn
             (and (vlax-property-available-p Obj 'RegenerateTableSuppressed)
                (vla-put-RegenerateTableSuppressed Obj :vlax-true)
              )
             (VL-CATCH-ALL-APPLY
               '(lambda (col row / i j)
                  (setq i '-1)
                  (repeat col
                    (setq i (1+ i) j '-1)
                    (repeat row
                      (setq j (1+ j))
                      (if (= (vla-GetCellType Obj j i) acTextCell)
                      (vla-settext obj j i
                      (_mtext-color-clean
                        (vla-gettext obj j i)
                        )
                        )
                        )
                      (vla-SetCellContentColor obj j i (vla-get-truecolor obj))
                      (vla-SetCellGridColor obj j i (+ acBottomMask acTopMask acLeftMask acRightMask) (vla-get-truecolor obj))
                      )
                    )
                  )
               (list (vla-get-Columns Obj)(vla-get-Rows Obj))
               )
             (and (vlax-property-available-p Obj 'RegenerateTableSuppressed)
                  (vla-put-RegenerateTableSuppressed Obj :vlax-false)
                  )
	   ) ;_ end of progn
	 )
      ) ;_ end of vlax-for
        )
      )
    ) ;_ end of if
  ) ;_ end of vlax-for
 (vl-cmdf "_regenall")
) ;_ end of defun

(defun C:BGCOLOR ( / doc col)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark doc)
  (bg:layer-status-save)
  (if (setq col (acad_colordlg 256))
    (progn
      (ChangeAllObjectsColor doc  col);_ 7 — номер цвета
      ;;;(ChangeXrefAllObjectsColor doc  col)
      )
    )
  ;;;Обработка стилей мультилиний
  (vl-catch-all-apply
    '(lambda ()
       (entmod
         (mapcar '(lambda (x / y)
                    (if (= (car x) 350)
                      (progn
                        (setq y (mapcar '(lambda (z)
                                           (if (= (car z) 62)
                                             (cons 62 col)
                                             z
                                           ) ;_ end of if
                                         ) ;_ end of lambda
                                        (entget (cdr x))
                                ) ;_ end of mapcar
                        ) ;_ end of setq
                        (cons 350 (entmakex y))
                      ) ;_ end of progn
                      x
                    ) ;_ end of if
                  ) ;_ end of lambda
                 (dictsearch (namedobjdict) "ACAD_MLINESTYLE")
         ) ;_ end of mapcar
       ) ;_ end of entmod
     ) ;_ end of lambda
  ) ;_ end of vl-catch-all-apply
  (bg:layer-status-restore)
  (vla-regen doc acallviewports)
  (vla-endundomark doc)
  (princ)
  )
;;; Returns the list of names of blocks with 1 primitive thing 
;;; возвращает список имен блоков с 1 примитивом (блоки A$C* или любые другие, состоящие из
;;; одного примитва и этот примитив - блок

(defun bg:block-list-one-item ( / Doc Blks blkname)
  (setq Doc (vla-get-activedocument (vlax-get-acad-object))
	Blks (vla-get-Blocks Doc)
	)
  (vlax-for Blk Blks
    (if (and (= (vla-get-IsXref Blk) :vlax-false)
	  (= (vla-get-IsLayout Blk) :vlax-false)
	  (= (vla-get-count Blk) 1)
;;;Берем только блоки "A$C*" и состоящие из другого блока
	  (or (wcmatch (vla-get-name Blk) "A$C*")
	      (= (vla-get-ObjectName (vla-item Blk 0)) "AcDbBlockReference")
	      )
          ;_ (= (vla-get-ObjectName (vla-item Blk 0)) "AcDbAttributeDefinition")
	  )
      (setq blkname (cons (vla-get-Name Blk) blkname))
      )
    )
    blkname
  )

(defun bg:block-GetXclip ( vla-obj / result )
;;;;

;;;http://www.cadtutor.net/forum/showthread.php?63267-Is-there-a-condition-statement-to-find-out-if-Xclip-was-used
;;;	;;;	(defun LM:XClipBoundary ( ename / xdict )
;;;	;;;	    (if
;;;	;;;	        (setq xdict (cdr (assoc 360 (entget ename))))
;;;	;;;	        (LM:XClipBoundary xdict)
;;;	;;;	        (if
;;;	;;;	            (and
;;;	;;;	                (eq "SPATIAL_FILTER" (cdr (assoc 0 (setq ename (entget ename)))))
;;;	;;;	                (eq 1 (cdr (assoc 71 ename)))
;;;	;;;	            )
;;;	;;;	            (   (lambda ( massoc ) (massoc 10 ename))
;;;	;;;	                (lambda ( key elist / item )
;;;	;;;	                    (if (setq item (assoc key elist))
;;;	;;;	                        (cons (cdr item) (massoc key (cdr (member item elist))))
;;;	;;;	                    )
;;;	;;;	                )
;;;	;;;	            )
;;;	;;;	        )
;;;	;;;	    )
;;;	;;;	)

;;;Через LISP без vla
;;;  (and (setq blk (car(entsel "\nУкажи подрезанный блок")))
;;;     (setq dict (cdr(assoc 360 (entget blk))))
;;;     (if (and
;;;           (setq sp (member '(3 . "ACAD_FILTER") (entget dict)))
;;;           (setq sp (cdr(assoc 360 sp)))
;;;           (setq sp (member '(3 . "SPATIAL")(entget sp)))
;;;           (setq sp (cdr(assoc 360 sp)))
;;;           )
;;;       (progn
;;;         (if (= (cdr(assoc 71 (entget sp))) 1)
;;;           (alert "Блок подрезан\nКонтур включен")
;;;           (alert "Блок подрезан\nКонтур ОТКЛЮЧЕН")
;;;         )
;;;         )
;;;       (alert "Блок не подрезан")
;;;         )
;;;     )
;;Если блок подрезан, возвращает SPATIAL, иначе nil
;;If the block is clipped, returns SPATIAL filter, differently return nil
;; test
;; (bg:block-GetXclip (vlax-ename->vla-object(car(entsel "\nSelect clipped xref"))))
  
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
;;;(defun bg:block-get-name (blkobj)
;;;(cond
;;;  ((and (vlax-property-available-p blkobj 'isdynamicblock)
;;;	(= (vla-get-isdynamicblock blkobj) :vlax-true)
;;;	) ;_ end of and
;;;   (vla-get-effectivename blkobj)
;;;   )
;;;  (t (vla-get-name blkobj))
;;;  ) ;_ end of cond
;;;  )

;;;	(defun LM:EffectiveName ( blockentity / name repbtag )
;;;	;;----------------=={ Effective Block Name }==----------------;;
;;;	;;                                                            ;;
;;;	;;  Returns the effective name of a block.                    ;;
;;;	;;------------------------------------------------------------;;
;;;	;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;;	;;------------------------------------------------------------;;
;;;	;;  Arguments:                                                ;;
;;;	;;  blockentity - Block Reference Entity name                 ;;
;;;	;;------------------------------------------------------------;;
;;;	;;  Returns:  True block name as per the block definition     ;;
;;;	;;------------------------------------------------------------;;
;;;	  
;;;	  (if (wcmatch (setq name (cdr (assoc 2 (entget blockentity)))) "`**")
;;;	    (if
;;;	      (and
;;;	        (setq repbtag
;;;	          (cdadr
;;;	            (assoc -3
;;;	              (entget
;;;	                (cdr
;;;	                  (assoc 330
;;;	                    (entget (tblobjname "BLOCK" name))
;;;	                  )
;;;	                )
;;;	               '("AcDbBlockRepBTag")
;;;	              )
;;;	            )
;;;	          )
;;;	        )
;;;	        (setq repbtag (handent (cdr (assoc 1005 repbtag))))
;;;	      )
;;;	      (setq name (cdr (assoc 2 (entget repbtag))))
;;;	    )
;;;	  )
;;;	  name
;;;	)

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

;;; Взрывает блоки, состоящие из одного примитива
;;; Explode blocks consisting of one primitive thing (it is possible other block)
(defun bg:block-explode-one-item ( / blklist count xcount tmp)
   (bg:layer-status-save)
  (setq blklist (bg:block-list-one-item) count 0 xcount 0)
  (vlax-for obj (vla-get-block(vla-item (vla-get-Layouts(vla-get-activedocument (vlax-get-acad-object)))(getvar "CTAB")))
    (if (and (eq (vla-get-ObjectName obj) "AcDbBlockReference")
	     (not (vlax-property-available-p obj 'path))
	     (vl-position (bg:block-get-name obj) blklist)
        )
      (progn
	(if (and (setq tmp (bg:block-GetXclip obj))
		 (= 1 (cdr(assoc 71 tmp)))
		 )
	  (progn
	    ;;; Здесь должна быть реакция на подрезанный блок
	    ;;; Игнорируем
	    (setq xcount (1+ xcount))
	    )
	 (progn
	   (bg:explode-block (vlax-vla-object->ename obj) nil)
	   (setq count (1+ count))
	   )
	)
      )
      )
    )
  (bg:layer-status-restore)
  (list count xcount)
  )

;;en -ename
;;Rdn - string name of appid or nil -all
;;Удалить все Расширенные данные ( XDATA РД )
(defun bg:del-XDATA-from-ename (en Rdn / elist sub)
  (if (= (type en) 'vla-object)
    (setq en (vlax-vla-object->ename en))
  )
  (setq elist (entget en (list "*")))
  (if (null Rdn)
    (progn
      (setq sub (mapcar 'car (cdr (assoc -3 elist))))
      (setq sub	(vl-remove-if
		  '(lambda (x) (wcmatch (strcase x) "ACAD*,ACDB*"))
		  sub
		)
      )
      (if sub
	(progn
	  (setq sub (list (cons -3 (mapcar 'list sub))))
	  (entmod (append (entget en) sub))
	)
      )
    )
    (progn
      (foreach i (cdr (assoc -3 elist))
	(if (not (wcmatch (strcase (car i)) (strcase Rdn)))
	  (setq sub (append sub (list i)))
	  (setq sub (append sub (list (list (car i)))))
	)
      )
      (setq sub	  (cons -3 sub)
	    elist (subst sub (assoc -3 elist) elist)
	    elist (entmod elist)
      )
    )
  )
)


(defun bg:get-all-atts (obj)
  (if (and obj
           (vlax-property-available-p obj 'Hasattributes)
	   (eq :vlax-true (vla-get-HasAttributes obj))
      )
    (vl-catch-all-apply
      (function
	(lambda	()
	  (mapcar (function (lambda (x)
			      (cons (vla-get-TagString x)
                                    (bg:FieldCode (vlax-vla-object->ename x))
                                    ;;; Не используем (vla-get-TextString x), т.к если атрибут содержит UNICOD символы
                                    ;;; например "Н\U+2082О", то (vla-get-TextString x) вернет "Н?О" + сохраняем, если есть поля
			      )
			    )
		  )
		  (append (vlax-invoke obj 'Getattributes)
			  (vlax-invoke obj 'Getconstantattributes)
		  )
	  )
	)
      )
    )
  )
)
(defun bg:set-atts-bylist (block att_list)
;; obj - Ename or Vla object of block
;; att_list - list ((Tag_Name1 . Value1)(Tag_Name2 . Value2) ...)
;;                 Tag_Name - string
;;                    Value - string
(if (= (type block) 'ENAME)(setq block (vlax-ename->vla-object block)))
(setq att_list (mapcar '(lambda(x)(cons (strcase (car x))(cdr x))) att_list))
 (if (and block
           (vlax-write-enabled-p block)
           (not(vlax-erased-p block))
           (= (vla-get-ObjectName block) "AcDbBlockReference")
           (vlax-property-available-p block 'Hasattributes)
	   (eq :vlax-true (vla-get-HasAttributes block))
      )
    (mapcar
    (function
      (lambda ( attrib / tmp )
        (if (setq tmp (assoc(strcase(vla-get-TagString attrib)) att_list))
          (progn
            (vla-put-TextString attrib " ")
            (vla-put-TextString attrib  (cdr tmp))
            )
        )
      )
    )
    (vlax-invoke block 'GetAttributes)
  )
 )
)

;;;	(defun bg:DynToStatic ( obj ConvertMode / i name ret attlist)
;;;	;;; obj - vla object
;;;	;;; ConvertMode - nil ConvertToAnonymousBlock
;;;	;;;             - t   ConvertToStaticBlock
;;;	;;; Return - t - if converted or nil if error  
;;;	;;;"ConvertToStaticBlock" or "ConvertToAnonymousBlock"
;;;	  (if (= (type obj) 'ENAME)
;;;	      (setq obj (vlax-ename->vla-object obj))
;;;	    )
;;;	  (setq attlist (bg:get-all-atts obj))
;;;	  (cond
;;;	    ((and ConvertMode
;;;	          (vlax-write-enabled-p obj)
;;;	          (vlax-method-applicable-p obj "ConvertToStaticBlock")
;;;	          )
;;;	     (setq name (strcat "$BG$_" (substr (rtos (getvar "CDATE") 2 9) 10) "_"))
;;;	     (setq i 1)
;;;	     (while (tblsearch "Block" (strcat name (itoa i)))(setq i (1+ i)))
;;;	     (setq name (strcat name (itoa i)))
;;;	     (setq ret
;;;	            (not
;;;	              (VL-CATCH-ALL-ERROR-P
;;;	                (vl-catch-all-apply 'vlax-invoke (list obj "ConvertToStaticBlock" name))
;;;	                )
;;;	              )
;;;	           )
;;;	     )
;;;	    ((and (not ConvertMode)
;;;	          (vlax-write-enabled-p obj)
;;;	          (vlax-method-applicable-p obj  "ConvertToAnonymousBlock")
;;;	          )
;;;	     (setq ret
;;;	            (not
;;;	              (VL-CATCH-ALL-ERROR-P
;;;	                (vl-catch-all-apply 'vlax-invoke (list obj  "ConvertToAnonymousBlock"))
;;;	                )
;;;	              )
;;;	           )
;;;	     )
;;;	    (t nil)
;;;	    )
;;;	  (if (and ret attlist)
;;;	    (progn
;;;	      (bg:set-atts-bylist obj attlist)
;;;	;;;      (command "_.Updatefield" (vlax-vla-object->ename obj) "")
;;;	;;;      (while (> (getvar "CMDACTIVE") 0) (command ""))
;;;	      )
;;;	    )
;;;	  ret
;;;	  )
(defun bg:DynToStatic ( obj ConvertMode / i name ret attlist)
;;; obj - vla object
;;; ConvertMode - nil ConvertToAnonymousBlock
;;;             - t   ConvertToStaticBlock
;;; Return - t - if converted or nil if error  
;;;"ConvertToStaticBlock" or "ConvertToAnonymousBlock"
  (if (= (type obj) 'ENAME)
      (setq obj (vlax-ename->vla-object obj))
    )
;;;  (setq attlist (bg:get-all-atts obj))
  (cond
    ((and ConvertMode
          (vlax-write-enabled-p obj)
          (vlax-method-applicable-p obj "ConvertToStaticBlock")
          )
     (setq name (strcat "$BG$_" (substr (rtos (getvar "CDATE") 2 9) 10) "_"))
     (setq i 1)
     (while (tblsearch "Block" (strcat name (itoa i)))(setq i (1+ i)))
     (setq name (strcat name (itoa i)))
     (setq ret
            (not
              (VL-CATCH-ALL-ERROR-P
                (vl-catch-all-apply 'vlax-invoke (list obj "ConvertToStaticBlock" name))
                )
              )
           )
     )
    ((and (not ConvertMode)
          (vlax-write-enabled-p obj)
          (vlax-method-applicable-p obj  "ConvertToAnonymousBlock")
          )
     (setq ret
            (not
              (VL-CATCH-ALL-ERROR-P
                (vl-catch-all-apply 'vlax-invoke (list obj  "ConvertToAnonymousBlock"))
                )
              )
           )
     )
    (t nil)
    )
  ;;; Add VVA 2014-10-08 v.2.28
(vl-catch-all-apply
  '(lambda()
     (vlax-map-Collection
       (vla-item (vla-get-Blocks(vla-get-activedocument (vlax-get-acad-object)))(vla-get-name obj))
       '(lambda (x)(if(eq(vla-get-Visible x) :vlax-false)(vla-delete x)))
       )
     )
  )                         

  ;;;VVA 2011-11-09 Remove
;;;  (if (and ret attlist)
;;;    (progn
;;;      (bg:set-atts-bylist obj attlist)
;;;      (command "_.Updatefield" (vlax-vla-object->ename obj) "")
;;;      (while (> (getvar "CMDACTIVE") 0) (command ""))
;;;      )
;;;    )
  ret
  )

(defun bg:ConvertDynBlock ( ConvertMode selectall / doc ss i *error* obj name j k str)
  ;;; ConvertMode - nil ConvertToAnonymousBlock
  ;;;             - t   ConvertToStaticBlock
  ;;; selectall   - t - select all
  ;;;             - nil - select
  ;;; Use (bg:ConvertDynBlock nil nil)
  ;;; Use (bg:ConvertDynBlock t nil)
   (defun *error* (msg)
     (princ msg)
     (vla-endundomark doc)
     (bg:layer-status-restore)
     )
  (vl-load-com)
  (if ConvertMode
    (setq str "ConvertToStaticBlock")
    (setq str "ConvertToAnonymousBlock")
    )
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (if (null selectall)
    (progn
  (princ
    (strcat "\n<< " str " >> "
    (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
      "Выберите динамические блоки"
      "Select dynamic blocks"
      )
     )
    )
  (setq ss (ssget "_I" '((0 . "Insert"))))
  )
    )
  (SSSETFIRST)
  (vla-startundomark doc)
  (bg:layer-status-save)
  (if
    (if selectall
      (setq ss (ssget "_X" '((0 . "Insert"))))
      (or ss (setq ss (ssget "_:L" '((0 . "Insert")))))
      )
    (repeat (setq k 0 i (sslength ss))
      (setq obj (vlax-ename->vla-object(ssname ss (setq i (1- i)))))
      (if (and (vlax-write-enabled-p obj)
               (= (vla-get-isdynamicblock obj) :vlax-true)
               (vlax-method-applicable-p obj str)
               (setq k (1+ k))
               )
        (bg:DynToStatic obj ConvertMode)
      )
    )
    )
  (bg:layer-status-restore)
  (vla-regen doc acallviewports)
  (vla-endundomark doc)
  (terpri)(princ str)(princ " - ")(princ k)
  (princ)
  )
(defun C:BGBLDYN2A ()
  ;;; Conver Dynamic Blocks to Anonymous
  (bg:ConvertDynBlock nil nil)
  )
(defun C:BGBLDYN2S ()
  ;;; Conver Dynamic Blocks to Static
  (bg:ConvertDynBlock t nil)
  )
 (defun C:BGBLALLDYN2A ()
  ;;; Conver ALL Dynamic Blocks to Anonymous
  (bg:ConvertDynBlock nil t)
  )
(defun C:BGBLALLDYN2S ()
  ;;; Conver ALL Dynamic Blocks to Static
  (bg:ConvertDynBlock t t)
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
            (prompt (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\nУказанный блок не подрезан" "\nThe specified block is not clipped"))
            )
          )
         (car ent)
         (cadr ent)
         )
       )
     (vla-endundomark actdoc)
     )
    (vl-catch-all-apply 'entsel
                        (list (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\nУкажите подрезанный блок: " "\nSelect clipped block"))
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
(defun c:BGBLCC () (bg:block-color) (princ))
(defun c:BGENCC () (pl:block-ent-color) (princ))
;;; Alaspher encc http://forum.dwg.ru/showthread.php?t=1036
;;; http://forum.dwg.ru/showpost.php?p=166220&postcount=18
(defun pl:block-ent-color (/ adoc color ent lays)
    (setq adoc   (vla-get-activedocument (vlax-get-acad-object))
          lays   (vla-get-layers adoc)
          color  (cond
                   ((and (listp *BG-COLOR*)(assoc 430 *BG-COLOR*))
                    (acad_truecolordlg (assoc 430 *BG-COLOR*))
                    )
                   ((and (listp *BG-COLOR*)(assoc 420 *BG-COLOR*))
                    (acad_truecolordlg (assoc 420 *BG-COLOR*))
                    )
                   ((and (listp *BG-COLOR*)(assoc 62 *BG-COLOR*))
                    (acad_truecolordlg (assoc 62 *BG-COLOR*))
                    )
                   (t (acad_truecolordlg 256))  ;_(acad_truecolordlg (assoc 420 color))
                   )
          *BG-COLOR* color
    )
    (if color
        (progn (setvar "errno" 0)
               (vla-startundomark adoc)
               (while (and (not (vl-catch-all-error-p
                                    (setq ent (vl-catch-all-apply
                                                  (function nentsel)
                                                  '("\nSelect entity <Exit>:")
                                              )
                                    )
                                )
                           )
                           (/= 52 (getvar "errno"))
                      )
                   (if ent
                       (progn (setq ent (vlax-ename->vla-object (car ent))
                                    lay (vla-item lays (vla-get-layer ent))
                              )
                              (if (= (vla-get-lock lay) :vlax-true)
                                  (progn (setq layloc (cons lay layloc))
                                         (vla-put-lock lay :vlax-false)
                                  )
                              )
                              ;(vl-catch-all-apply (function vla-put-color) (list ent color))
                              (vl-catch-all-apply (function bg:put-color) (list ent color))
                              (vla-regen adoc acallviewports)
                       )
                       (princ "\nNothing selection! Try again.")
                   )
               )
               (foreach i layloc (vla-put-lock i :vlax-true))
               (vla-endundomark adoc)
        )
    )
    (princ)
)
;;;На основе Alaspher blcc encc http://forum.dwg.ru/showthread.php?t=1036
(defun bg:block-color (/ adoc blocks color ins lays ss e2)
    (setq adoc   (vla-get-activedocument (vlax-get-acad-object))
          blocks (vla-get-blocks adoc)
          lays   (vla-get-layers adoc)
          color  (cond
                   ((and (listp *BG-COLOR*)(assoc 430 *BG-COLOR*))
                    (acad_truecolordlg (assoc 430 *BG-COLOR*))
                    )
                   ((and (listp *BG-COLOR*)(assoc 420 *BG-COLOR*))
                    (acad_truecolordlg (assoc 420 *BG-COLOR*))
                    )
                   ((and (listp *BG-COLOR*)(assoc 62 *BG-COLOR*))
                    (acad_truecolordlg (assoc 62 *BG-COLOR*))
                    )
                   (t (acad_truecolordlg 256))  ;_(acad_truecolordlg (assoc 420 color))
                   )
          *BG-COLOR* color
    )
    (if color
        (progn (setvar "errno" 0)
               (vla-startundomark adoc)
               (while (and (not (vl-catch-all-error-p
                                    (setq ins (vl-catch-all-apply
                                                  (function entsel)
                                                    (list (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                                                            "\nВыберите блок <Выход>:"
                                                            "\nSelect block <Exit>:")
                                                          )
                                              )
                                    )
                                )
                           )
                           (/= 52 (getvar "errno"))
                      )
                   (if ins
                       (progn
                         (setq ss (vlax-ename->vla-object (car ins)))
                              (if  (= (vla-get-objectname ss) "AcDbBlockReference")
                                  (if (vlax-property-available-p ss 'path)
                                      (princ "\nThis is external reference! Try pick other.")
                                      (progn
					(bg:layer-status-save)
					(_pl:block-color blocks ss color lays)
					(if (setq ss nil
                                                  ss (ssget "_X"
                                                            (list
                                                              (cons 2
                                                                (strcat
                                                                  (if (wcmatch(cdr(assoc 2 (entget(car ins)))) "`**")
                                                                    "`"
                                                                    ""
                                                                    )
                                                                  (cdr(assoc 2 (entget(car ins))))
                                                                  )
                                                                )
                                                              (cons 66 1)
                                                              (cons 410 (getvar "CTAB"))
                                                              )
                                                            )
                                                  )
					  (progn
					    (foreach blk (mapcar (function vlax-ename->vla-object) (vl-remove-if (function listp)
							  (mapcar (function cadr) (ssnamex ss))))
					      (foreach at (vlax-invoke blk 'Getattributes)
;;;						(vl-catch-all-apply
;;;                                                  (function vla-put-color)(list at color))
                                                (vl-catch-all-apply
                                                  (function
                                                    (lambda()
                                                      (entmod
                                                        (append
                                                          (vl-remove-if '(lambda(x)(member(car x) '(62 420 430)))
                                                            (entget(vlax-vla-object->ename at))
                                                            )
                                                          color
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        (bg:layer-status-restore)
                                        (vla-regen adoc acallviewports)
                                        )
                                    )
                                  (princ "\nThis isn't block! Try pick other.")
                                )
                         )
                       (princ "\nNothing selection! Try again.")
                   )
               )
               (vla-endundomark adoc)
        )
    )
    (princ)
)
(defun bg:put-color ( e color / TrueColor)
  ;;; e - vla-object
  ;;; color - list what return acad_truecolordlg
(if (equal (vla-get-ObjectName e) "AcDbZombieEntity")
  (princ "\nskip proxy entities ")
(cond
        ((and
           (= color -1)
           (not(eq (vla-get-color e) acByBlock))
           (not(eq (vla-get-color e) acByLayer))
           )
         (setq TrueColor (vla-get-truecolor e))
         (vla-setrgb TrueColor
           (vla-get-red TrueColor)
           (vla-get-green TrueColor)
           (vla-get-blue TrueColor)
           ) ;_ end of vla-setRGB
         (vl-catch-all-apply (function vla-put-Truecolor) (list e TrueColor))
         )
        ((listp color)
          (vl-catch-all-apply
            (function
              (lambda ()
                (entupd
                  (cdr
                    (assoc
                      -1
                      (entmod
                        (append
                          (vl-remove-if
                            '(lambda (x) (member (car x) '(62 420 430)))
                            (entget (vlax-vla-object->ename e))
                          ) ;_ end of vl-remove-if
                          color
                        ) ;_ end of append
                      ) ;_ end of entmod
                    ) ;_ end of assoc
                  ) ;_ end of cdr
                ) ;_ end of entupd
              ) ;_ end of lambda
            ) ;_ end of function
          ) ;_ end of vl-catch-all-apply
          (vl-catch-all-apply (function vla-update) (list e))
        )
        ((numberp color)
         (vl-catch-all-apply (function vla-put-color) (list e color))
         )
        (t nil)
        )
  )
   )
(defun _pl:block-color (blocks ins color lays / lay layfrz layloc c ee TrueColor)
  ;;;color >0 - ACI color
  ;;;      -1 - convert to RGB
  
    (vlax-for e (vla-item
                  blocks
                  (vla-get-name ins)
;;;                  (if (vlax-property-available-p ins 'EffectiveName)
;;;                    (vla-get-effectivename ins)
;;;                    (vla-get-name ins)
;;;                  ) ;_ end of if
                ) ;_ end of vla-item
        (setq lay (vla-item lays (vla-get-layer e)))
        (if (= (vla-get-freeze lay) :vlax-true)
            (progn (setq layfrz (cons lay layfrz)) (vla-put-freeze lay :vlax-false))
        )
        (if (= (vla-get-lock lay) :vlax-true)
            (progn (setq layloc (cons lay layloc)) (vla-put-lock lay :vlax-false))
        )
      (bg:put-color e color)
        (if (and (= (vla-get-objectname e) "AcDbBlockReference")
                 (not (vlax-property-available-p e 'path))
            )
            (_pl:block-color blocks e color lays)
        )
        (foreach i layfrz (vla-put-freeze i :vlax-true))
        (foreach i layloc (vla-put-lock i :vlax-true))
    )
)

(defun bg:DetachImage (ImgName)
        (vl-catch-all-apply
            '(lambda ()
                 (vla-delete
                     (vla-item
                         (vla-item
                             (vla-get-dictionaries
                                 (vla-get-activedocument (vlax-get-acad-object))
                             ) ;_ end of vla-get-dictionaries
                             "ACAD_IMAGE_DICT"
                         ) ;_ end of vla-Item
                         ImgName
                     ) ;_ end of vla-Item
                 ) ;_ end of vla-Delete
             ) ;_ end of lambda
        ) ;_ end of vl-catch-all-apply
    ) ;_ end of defun

;;;Удаление всего с выключенных и замороженных слоев
(defun bg:delete-from-layer (/			 layer-list
			     aDOC		 count
			     *error*		 all_raster_image_name
			     used_raster_image_name
			     ignore-block-list
			    )
  ;;;Удаление всего с выключенных и замороженных слоев
  (defun *error* (msg)
    (setvar "MODEMACRO" "")
    (princ msg)
    (vla-regen aDOC acactiveviewport)
    (bg:progress-clear)
    (princ)
  ) ;_ end of defun
  (defun _loc-delete-items ()
    (if	(= (vla-get-IsXref Blk) :vlax-false)
      (progn
	(setq count 0)
	(if (> (vla-get-count Blk) 100)
	  (bg:progress-init
	    (strcat (vla-get-name Blk) " :")
	    (vla-get-count Blk)
	  ) ;_ end of bg:progress-init
	  (progn
	    (setvar "MODEMACRO" (vla-get-name Blk))
	  ) ;_ end of progn
	) ;_ end of if
	(vlax-for Obj Blk
	  (if (and
		(= (vla-get-ObjectName Obj) "AcDbRasterImage")
		(vlax-property-available-p obj 'Name)
		(not (vl-catch-all-error-p
		       (vl-catch-all-apply 'vla-get-name (list obj))
		     ) ;_ end of vl-catch-all-error-p
		) ;_ end of not
		(vlax-property-available-p obj 'ImageFile)
	      ) ;_ end of and
	    (setq used_raster_image_name
		   (cons (vla-get-Name Obj)
			 used_raster_image_name
		   ) ;_ end of cons
	    ) ;_ end of setq
	  ) ;_ end of if
;;;В таблице описания блока оставляем примитивы на 0 слое в любом случае
	  (if (or
		(eq (vla-get-IsLayout Blk) :vlax-true)
		(and (eq (vla-get-IsLayout Blk) :vlax-false)
		     (= (vla-get-ObjectName Blk) "AcDbBlockTableRecord")
		     (/= (vla-get-layer Obj) "0")
		) ;_ end of and
	      ) ;_ end of or
	    (vl-catch-all-apply
	      '(lambda ()
		 (bg:progress (setq count (1+ count)))
		 (if (vlax-write-enabled-p Obj)
		   (if
		     (and (vlax-property-available-p Obj 'Layer)
			  (vl-position (vla-get-layer Obj) layer-list)
		     ) ;_ end of and
						  ;ObjectName (RO) = "AcDbBlockReference"
						  ;ObjectName (RO) = "AcDbBlockTableRecord"
		      (if (= (vla-get-ObjectName Obj)
			     "AcDbBlockReference"
			  ) ;_ end of =
			(if (not (vl-position
				   (vla-get-name Obj)
				   ignore-block-list
				 ) ;_ end of vl-position
			    ) ;_ end of not
			  (vl-catch-all-apply 'vla-delete (list Obj)) ;_ Delete BlockReference object
			) ;_ end of if
			(vl-catch-all-apply 'vla-delete (list Obj)) ;_ Delete OTHER object
		      ) ;_ end of if
		      (vl-catch-all-apply
			'bg:del-XDATA-from-ename
			(list Obj nil)
		      ) ;_Delete Appid from Object
		   ) ;_ end of if
		 ) ;_ end of if
	       ) ;_ end of lambda
	    ) ;_ end of vl-catch-all-apply
	  ) ;_ end of if
	) ;_ end of vlax-for
	(bg:progress-clear)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun
  (setq	layer-list nil
	aDOC	   (vla-get-activedocument (vlax-get-acad-object))
  ) ;_ end of setq
;;;  (grtext -1 "Stage 1. Viewing of layers")
  (vlax-for item (vla-get-layers aDOC)
    (if	(or (= (vla-get-freeze item) :vlax-true)
	    (= (vla-get-layeron item) :vlax-false)
	) ;_ end of or
      (setq layer-list (cons (vla-get-name item) layer-list))
    ) ;_ end of if
  ) ;_ end of vlax-for
  (bg:layer-status-save)
  (setq layer-list (vl-remove-if-not 'snvalid layer-list))
;;;  (grtext -1 "Stage 1. Viewing of layers")
  (if layer-list
    (progn
      (setq ignore-block-list nil)
      (vlax-for	Blk (vla-get-Blocks aDOC)
;;;2 цикла: по описаниям блоков и по листам
;;; ignore-block-list - список блоков
;;; имеющих хотя бы один элемент на включенном слое
	(if (eq (vla-get-IsLayout Blk) :vlax-false) ;_ 1-й цикл по блокам
	  (progn
	    (_loc-delete-items)
	    (setq count nil)
	    (vlax-for Obj Blk
	      (if
		(and (/= (vla-get-layer Obj) "0")
		     (not (vl-position (vla-get-layer Obj) layer-list))
		) ;_ end of and
		 (setq ignore-block-list
			(cons (vla-get-Name Blk)
			      ignore-block-list
			) ;_ end of cons
		 ) ;_ end of setq
	      ) ;_ end of if
	    ) ;_ end of vlax-for

	  ) ;_ end of progn
	) ;_ end of if
      ) ;_ end of vlax-for
;;;2 цикла: по описаниям блоков и по листам
      (vlax-for	Blk (vla-get-Blocks aDOC)
;;; ignore-block-list - список блоков
;;; имеющих хотя бы один элемент на включенном слое
	(if (eq (vla-get-IsLayout Blk) :vlax-true)
	  (_loc-delete-items)
	) ;_ end of if
      ) ;_ end of vlax-for
    ) ;_ end of progn
  ) ;_ end of if
;;;  (grtext -1 "Stage 3. Audit and Purge")
  (vla-auditinfo aDOC :vlax-true)
;;;Audit
  (repeat 3 (vla-purgeall aDOC))
  (if (and layer-list
    (setq all_raster_image_name
	   (mapcar 'cdr
		   (vl-remove-if-not
		     (function (lambda (x) (= 3 (car x))))
		     (dictsearch (namedobjdict) "ACAD_IMAGE_DICT")
		   ) ;_ end of vl-remove-if-not
	   ) ;_ end of mapcar
    ) ;_ end of setq
           )
     (setq all_raster_image_name (mapcar 'strcase all_raster_image_name))
  ) ;_ end of if
  (setq used_raster_image_name (mapcar 'strcase used_raster_image_name))
  (mapcar
    '(lambda (img)
       (setq all_raster_image_name
	      (vl-remove img all_raster_image_name)
       ) ;_ end of setq
     ) ;_ end of lambda
    used_raster_image_name
  ) ;_ end of mapcar
  (mapcar 'bg:DetachImage all_raster_image_name)
;;;  (vl-cmdf "_.Redrawall")
    (vla-regen aDOC acActiveViewport)

  (setq *BG_LAYER_LST* nil)
) ;_ end of defun
(defun bg:conv-to-str (dat)
  (cond ((= (type dat) 'INT)(setq dat (itoa dat)))
         ((= (type dat) 'REAL)(setq dat (rtos dat 2 12)))
        ((null dat)(setq dat ""))
        (t (setq dat (vl-princ-to-string dat)))))  
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
   ;-----------------------------------------------------
   ; See if a block is explodable. Return T if it is, 
   ; otherwise return nil
   ;-----------------------------------------------------
 
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

(defun bg:del-from-list ( pat lst / tmp )
  (foreach item lst
    (if (/= (car item) pat)
      (setq tmp (cons item tmp))
      )
    )
    (reverse tmp)
  )
;;;По аналогии с BURST-ONE
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

;;; Сприсок вложенных блоков вложеные блоки nested block
;;; list of nested block
;;; http://www.cadtutor.net/forum/showthread.php?t=48702
;;; http://www.theswamp.org/index.php?topic=40367.0
;;; см.BlockCount V1-2.lsp Lee MAc
;;;		(defun c:NestSel ( / ss def n l )
;;;		  ;; © Lee Mac  ~  03.06.10
;;;		  (vl-load-com)
;;;
;;;		  (while (setq def (tblnext "BLOCK" (not def)))
;;;		    (if (ContainsNested (tblobjname "BLOCK" (setq n (cdr (assoc 2 def)))))
;;;		      (setq l (cons n l))
;;;		    )
;;;		  )
;;;
;;;		  (if l
;;;		    (sssetfirst nil
;;;		      (ssget "_X"
;;;		        (list '(0 . "INSERT") (cons 2 (lst->str l ",")))
;;;		      )
;;;		    )
;;;		  )
;;;		  (princ)
;;;		)
;;;
;;;		(defun ContainsNested ( ent / foo )
;;;		  ;; © Lee Mac  ~  03.06.10
;;;
;;;		  (defun foo ( e )
;;;		    (if (setq e (entnext e))
;;;		      (cons e (foo e))
;;;		    )
;;;		  )
;;;
;;;		  (vl-some
;;;		    (function
;;;		      (lambda ( x )
;;;		        (eq "INSERT" (cdr (assoc 0 (entget x))))
;;;		      )
;;;		    )
;;;		    (foo ent)
;;;		  )
;;;		)
;;;
;;;		(defun lst->str ( lst del )
;;;		  ;; © Lee Mac  ~  03.06.10
;;;		  (if (cdr lst)
;;;		    (strcat (car lst) del (lst->str (cdr lst) del))
;;;		    (car lst)
;;;		  )
;;;		) 
;;http://forum.dwg.ru/showthread.php?t=8346 ( И опять VLA-SELECT )
;;http://forums.augi.com/showthread.php?p=738077#post738077

;_Взрывает блок BURST'ом и возвращает список полученных объектов VLA
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
;;; blk - Ename блока
;;; level - t all level nil -one level
(defun bg:explode-block ( blk level / adoc csp blk_obj)
;_Цикл по примитивам блока
;_Необходим, чтобы взорвать вложенные блоки
;_ blk - Ename блока
;;; level - t all level nil -one level
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

;;Пример использования - все примитивы взорванного блока делаются красным
;;;(defun c:TEST ( / blk blk_obj)
;;;(setq blk (car(entsel "\nУкажите блок")))
;;;  ;_Чистим набор
;;;  (setq *ssRET* nil *ssRET*  (ssadd))
;;;  (bg:explode-block blk t)
;;;  (command "_CHANGE" *ssRET* "" "_P" "_C" 1 "")
;;;  (princ))
;;;
;;published by kpblc
;;http://www.arcada.com.ua/forum/viewtopic.php?t=526
;;modyfied and published by VVA
;;http://www.cadtutor.net/forum/showthread.php?t=13295
;|=============================================================================
*    Функция "нормализации" блоков активного файла. В зависимости от ответа на
* вопрос к типу ByBlock приводится либо тип линии, либо вес линии, либо цвет,
* либо все вместе (при пустом ответе, т.е. по умолчанию).
*    Обрабатываются все блоки, без исключения.
*  Не обрабатываются внешние ссылки
=============================================================================|;

(defun C:BGBLFIX ( / lst )
  (if (setq lst (bg:blfixdialog))
    (bg:blfix lst)
    )
  (princ)
  )
(defun C:BGLAYDEL ( )(BGLAYDEL nil))

(defun BGLAYDEL ( ask / mdm)
  ;;;ask - nil запрос t - удаление
  ;;;Удаление выключенных и замороженных слоев с примитивами
  (if
    (or ask
    (if	(= (getvar "DWGCODEPAGE") "ANSI_1251")
      (bg:msg-yes-no
	"Внимание"
	"Вы действительно хотите удалить \nвыключенные и замороженные слои\n и объекты на них?"
      )
      (bg:msg-yes-no
	"Attension"
	"You really want to remove \nfrozen and off layers and objects on it?"
      )
    )
        )
     (progn
       (command "_.UNDO" "_Mark")
       (setvar
         "CLAYER"
         (cdr
           (assoc
             2
             (entmod
               (subst
                 (cons
                   70
                   (boole
                     2
                     (cdr (assoc 70 (entget (tblobjname "layer" "0"))))
                     1
                   ) ;_ end of boole
                 ) ;_ end of cons
                 (assoc 70 (entget (tblobjname "layer" "0")))
                 (entget (tblobjname "layer" "0"))
               ) ;_ end of subst
             ) ;_ end of entmod
           ) ;_ end of assoc
         ) ;_ end of cdr
       ) ;_ end of setvar
       (setq mdm (getvar "MODEMACRO"))
       (bg:delete-from-layer)
       (setvar "MODEMACRO" mdm)
       (command "_.Regenall")
       (princ "\n*** Command _.UNDO _Back restore your layers")
       (princ)
     )
  )
  (princ)
)
;;;BG Color ATTribute
(defun C:BGCATT ( / ss i obj color *error* adoc)
   (defun *error* (msg)
     (setvar "NOMUTT" 0)
     (princ msg)
     (bg:layer-status-restore)
     (vla-endundomark adoc)(princ))
  (setq adoc  (vla-get-activedocument (vlax-get-acad-object))
   i '-1)
  (vla-startundomark adoc)
   (if (and (setq color  (cond
                   ((and (listp *BG-COLOR*)(assoc 430 *BG-COLOR*))
                    (acad_truecolordlg (assoc 430 *BG-COLOR*))
                    )
                   ((and (listp *BG-COLOR*)(assoc 420 *BG-COLOR*))
                    (acad_truecolordlg (assoc 420 *BG-COLOR*))
                    )
                   ((and (listp *BG-COLOR*)(assoc 62 *BG-COLOR*))
                    (acad_truecolordlg (assoc 62 *BG-COLOR*))
                    )
                   (t (acad_truecolordlg 256))  ;_(acad_truecolordlg (assoc 420 color))
                   )
          *BG-COLOR* color)
            (setvar "NOMUTT" 1)
            (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
               (princ "\nВыберите блоки с атрибутами <все>:")
               (princ "\nSelect Block with Attribute <all>:")
              )
            (or (setq ss (ssget "_:L" '((0 . "INSERT")(66 . 1))))
                (not (setvar "NOMUTT" 0))
                (setq ss (ssget "_X" (list '(0 . "INSERT")'(66 . 1)(cons 410 (getvar "CTAB")))))
                )
            (setvar "NOMUTT" 0)
            )
    (progn
      (bg:layer-status-save)
      (repeat (sslength ss)
        (setq Obj (vlax-ename->vla-object(ssname ss (setq i (1+ i)))))
        (if (and (= (vla-get-objectname Obj) "AcDbBlockReference")
                 (not (vlax-property-available-p Obj 'path))
                   )
            (foreach att (vlax-invoke Obj 'getattributes)
              ;;; (vla-put-color att Color) ;;;ZZZ1
              (bg:put-color att color)
            ) ;_ end of foreach
          )
	)
      (setvar "NOMUTT" 0)
      (bg:layer-status-restore)
      )
    )
  (vla-endundomark adoc)
  (princ)
  )

(defun C:BGCBL2 (/ ss i obj color *error* adoc blocks)
  (defun *error* (msg)
    (setvar "NOMUTT" 0)
    (princ msg)
    (bg:layer-status-restore)
    (vla-endundomark adoc)
    (princ)
  ) ;_ end of defun

(setq adoc  (vla-get-activedocument (vlax-get-acad-object))
     blocks (vla-get-blocks adoc)
        i '-1)
  (vla-startundomark adoc)
  (if
    (and (setq color  (cond
                   ((and (listp *BG-COLOR*)(assoc 430 *BG-COLOR*))
                    (acad_truecolordlg (assoc 430 *BG-COLOR*))
                    )
                   ((and (listp *BG-COLOR*)(assoc 420 *BG-COLOR*))
                    (acad_truecolordlg (assoc 420 *BG-COLOR*))
                    )
                   ((and (listp *BG-COLOR*)(assoc 62 *BG-COLOR*))
                    (acad_truecolordlg (assoc 62 *BG-COLOR*))
                    )
                   (t (acad_truecolordlg 256))  ;_(acad_truecolordlg (assoc 420 color))
                   )
          *BG-COLOR* color)
         ;;;(setq color '-1)
         (setvar "NOMUTT" 1)
         (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
           (princ "\nВыберите блоки <все>:")
           (princ "\nSelect Block <all>:")
         ) ;_ end of if
         (or (setq ss (ssget "_:L" '((0 . "INSERT"))))
             (not (setvar "NOMUTT" 0))
             (setq
               ss (ssget "_X"
                         (list '(0 . "INSERT") (cons 410 (getvar "CTAB")))
                  ) ;_ end of ssget
             ) ;_ end of setq
         ) ;_ end of or
         (setvar "NOMUTT" 0)
    ) ;_ end of and
     (progn
       (bg:layer-status-save)
       (repeat (sslength ss)
         (setq Obj (vlax-ename->vla-object (ssname ss (setq i (1+ i)))))
         (if (and (= (vla-get-objectname Obj) "AcDbBlockReference")
                  (not (vlax-property-available-p Obj 'path))
             ) ;_ end of and
           (vlax-for e (vla-item blocks (vla-get-name Obj))
             (if (and (= (vla-get-objectname e) "AcDbBlockReference")
                      (not (vlax-property-available-p e 'path))
                 ) ;_ end of and
               (progn
               (_pl:block-color blocks e color (vla-get-layers adoc))
               (entmod
                 (append
                   (vl-remove-if '(lambda(x)(member(car x) '(62 420 430)))
                     (entget(vlax-vla-object->ename e))
                     )
                   color
                   )
                 )
               )
             ) ;_ end of if
           ) ;_ end of vlax-for
         ) ;_ end of if
       ) ;_ end of repeat
       (setvar "NOMUTT" 0)
       (bg:layer-status-restore)
     ) ;_ end of progn
  ) ;_ end of if
  (vla-endundomark adoc)
  (vla-regen adoc acallviewports)
  (princ)
) ;_ end of defun
;;; Взрывает блоки, состоящие из одного примитива (возможно другого блока)
(defun C:BG1BLEXP ( / count xcount)
  (setq count (bg:block-explode-one-item)
	xcount (cadr count)
	count (car count)
	)
  
  (princ "\nExploded ")
  (princ count)
  (princ " blocks with one element. Found ")
  (princ xcount)
  (princ " blocks has xclip boundary.")
  (princ)
  )
;;Показыавет подрезанные блоки
(defun C:BGBLXCLIP ( / ss xss i blk)
  (setq i '-1 xss (ssadd))
  (if (setq ss (ssget "_X" (list '(0 . "INSERT")(cons 410 (getvar "CTAB")))))
    (progn
      (repeat (sslength ss)
	(if (bg:block-GetXclip (vlax-ename->vla-object(setq blk (ssname ss (setq i (1+ i))))))
	  (ssadd blk xss))
	)
      (sssetfirst nil xss)
      )
    )
  (princ "\nFound ")
  (princ (sslength xss))
    (princ " blocks has xclip boundary.")
  (setq ss nil xss nil)
  (princ)
  )

;;;Взрывает блоки 1-го уровня, у которых нет видимых атрибутов
(defun C:BGBLEXP1NOATT ( / i ss blk count xcount tmp *error*)
      (defun *error* (msg)
    (vla-EndUndoMark (vla-get-activedocument (vlax-get-acad-object)))
    (setvar "MODEMACRO" "")
    (princ msg)
    (vla-regen (vla-get-activedocument (vlax-get-acad-object)) acactiveviewport)
    (bg:progress-clear)
    (princ)
  ) ;_ end of defun

  (setq i '-1 count 0 xcount 0)
  (setq ss (ssget "_I" (list '(0 . "INSERT")'(66 . 0)(cons 410 (getvar "CTAB")))))
  (SSSETFIRST)
  (command "_.UNDO" "_Mark")
  (if (or
    ss
    (setq ss (ssget "_:L" (list '(0 . "INSERT")'(66 . 0)(cons 410 (getvar "CTAB")))))
    )
    ;(setq ss (ssget "_:L" (list '(0 . "INSERT")'(66 . 0)(cons 410 (getvar "CTAB")))))
    
    (progn
      (vla-StartUndoMark (vla-get-activedocument (vlax-get-acad-object)))
      (if (> (sslength ss) 50)
	  (bg:progress-init
	    (strcat "Working ...")
	    (sslength ss)
	  ) ;_ end of bg:progress-init
	) ;_ end of if
      (repeat (sslength ss)
	(if (and (setq tmp(bg:block-GetXclip (vlax-ename->vla-object(setq blk (ssname ss (setq i (1+ i)))))))
		 (= 1 (cdr(assoc 71 tmp)))
		 )
	  (setq xcount (1+ xcount))
	  (progn
            (bg:progress (setq count (1+ count)))
	    (bg:explode-block blk nil)
	    )
	  )
	)
      (vla-EndUndoMark (vla-get-activedocument (vlax-get-acad-object)))
      )
    )
  (princ "\nExploded ")
  (princ count)
  (princ " blocks. Found ")
  (princ xcount)
  (princ " blocks has xclip boundary.")
  (if(not(zerop count))(princ "\n*** Command _.UNDO _Back restore your drawing"))
  (princ)
  )
(defun C:BGBLDYNEXP1 (  / i ss blk count xcount tmp *error*)
    (defun *error* (msg)
    (vla-EndUndoMark (vla-get-activedocument (vlax-get-acad-object)))
    (setvar "MODEMACRO" "")
    (princ msg)
    (vla-regen (vla-get-activedocument (vlax-get-acad-object)) acactiveviewport)
    (bg:progress-clear)
    (princ)
  ) ;_ end of defun
  (setq count 0 xcount 0)
  (setq ss (ssget "_I" (list '(0 . "INSERT")(cons 410 (getvar "CTAB")))))
  (SSSETFIRST ss)
  (command "_.UNDO" "_Mark")
  (if (or ss (setq ss (ssget "_:L" (list '(0 . "INSERT")(cons 410 (getvar "CTAB"))))))
    (progn
      (vla-StartUndoMark (vla-get-activedocument (vlax-get-acad-object)))
      (if (> (sslength ss) 50)
	  (bg:progress-init
	    (strcat "Working ...")
	    (sslength ss)
	  ) ;_ end of bg:progress-init
	) ;_ end of if
      (repeat (setq i (sslength ss))
	(if (and (setq tmp(bg:block-GetXclip (vlax-ename->vla-object(setq blk (ssname ss (setq i (1- i)))))))
		 (= 1 (cdr(assoc 71 tmp)))
		 )
	  (setq xcount (1+ xcount))
	  (progn
            (bg:progress (setq count (1+ count)))
            (if (eq (vla-get-IsDynamicBlock (vlax-ename->vla-object blk)) :vlax-true)
              (bg:explode-block blk nil)
              )
	    )
	  )
	)
      (bg:progress-clear)
      (vla-EndUndoMark (vla-get-activedocument (vlax-get-acad-object)))
      )
    )
  (princ "\nTry to explode ")
  (princ count)
  (princ " blocks. Found ")
  (princ xcount)
  (princ " blocks has xclip boundary.")
  (if(not(zerop count))(princ "\n*** Command _.UNDO _Back restore your drawing"))
  (princ)
  )
;;;Взрывает блоки 1-го уровня, превращая видимые атрибуты в текст
(defun C:BGBLEXP1 ( / ret) (setq ret(BGBLEXP nil))
  (princ "\nTry to explode ")
  (princ (car ret))
  (princ " blocks. Found ")
  (princ (cadr ret))
  (princ " blocks has xclip boundary.")
  (if(not(zerop (car ret)))(princ "\n*** Command _.UNDO _Back restore your drawing"))
  (princ)

  )

;;;Взрывает блок и все блоки, входящие в него, превращая видимые атрибуты в текст
(defun C:BGBLEXP ( / ret )
  (setq ret(BGBLEXP t))
  (princ "\nTry to explode ")
  (princ (car ret))
  (princ " blocks. Found ")
  (princ (cadr ret))
  (princ " blocks has xclip boundary.")
  (if(not(zerop (car ret)))(princ "\n*** Command _.UNDO _Back restore your drawing"))
  (princ)
  )
(defun BGBLEXP ( level / i ss blk count xcount tmp *error*)
  ;;; t - all nil - one
    (defun *error* (msg)
    (vla-EndUndoMark (vla-get-activedocument (vlax-get-acad-object)))
    (setvar "MODEMACRO" "")
    (princ msg)
    (vla-regen (vla-get-activedocument (vlax-get-acad-object)) acactiveviewport)
    (bg:progress-clear)
    (princ)
  ) ;_ end of defun

  (setq count 0 xcount 0)
  (setq ss (ssget "_I" (list '(0 . "INSERT")(cons 410 (getvar "CTAB")))))
  (SSSETFIRST ss)
  (command "_.UNDO" "_Mark")
  (if (or ss (setq ss (ssget "_:L" (list '(0 . "INSERT")(cons 410 (getvar "CTAB"))))))
    (progn
      (vla-StartUndoMark (vla-get-activedocument (vlax-get-acad-object)))
      (if (> (sslength ss) 50)
	  (bg:progress-init
	    (strcat "Working ...")
	    (sslength ss)
	  ) ;_ end of bg:progress-init
	) ;_ end of if
      (repeat (setq i (sslength ss))
	(if (and (setq tmp(bg:block-GetXclip (vlax-ename->vla-object(setq blk (ssname ss (setq i (1- i)))))))
		 (= 1 (cdr(assoc 71 tmp)))
		 )
	  (setq xcount (1+ xcount))
	  (progn
            (bg:progress (setq count (1+ count)))
	    (bg:explode-block blk level)
	    )
	  )
	)
      (bg:progress-clear)
      (vla-EndUndoMark (vla-get-activedocument (vlax-get-acad-object)))
      )
    )
  (list count xcount)
  )
(defun C:BGCFTSEL( / *error* Doc ss CountField)
  ;;; http://forum.dwg.ru/showthread.php?t=20190&page=2
   (vl-load-com)  
  (defun *error* (msg)(princ msg)(vla-endundomark doc)(princ))
  (setq Doc (vla-get-activedocument (vlax-get-acad-object)))
 (vla-startundomark Doc)
  (if (setq ss (ssget "_:L"))
    (progn
      (setq CountField 0)
     (foreach obj (mapcar (function vlax-ename->vla-object)
	            (vl-remove-if (function listp)
		      (mapcar (function cadr) (ssnamex ss))))
       (setq CountField (ClearFieldInThisObject Doc Obj CountField))
       )
      (princ "\nConverting Field in ")(princ CountField)
      (princ " text's")
      )
    )
(vla-endundomark Doc)
(command "_.Regenall")  
  )

(defun C:BGCFT ()(ConvField->Text t)) ;;;C:CFT
;;; (defun C:CFTAll ()(ConvField->Text nil))
(defun ConvField->Text ( Ask / Doc *error* ClearFieldInAllObjects)
;;; t - Ask user nil - convert
;;; Как все поля чертежа сразу преобразовать в текст?
;;; Convert Field to Text
;;; Posted Vladimir Azarko (VVA)
;;; http://forum.dwg.ru/showthread.php?t=20190&page=2
;;; http://forum.dwg.ru/showthread.php?t=20190
  (vl-load-com)  
  (defun *error* (msg)(princ msg)
   (bg:layer-status-restore)
   (vla-endundomark doc)(princ)
  )
 
  (defun ClearFieldInAllObjects (Doc / txtstr tmp txt count CountField mtlist)
  (setq  CountField 0)  
  (vlax-for Blk	(vla-get-Blocks Doc)
    (if	(equal (vla-get-IsXref Blk) :vlax-false) ;;;kpbIc http://forum.dwg.ru/showpost.php?p=396910&postcount=30
      (progn
	(setq count 0
	      txt (strcat "Changed " (vla-get-name Blk))
	      )
	(grtext -1 txt)
;;;        (terpri)(princ "=================== ")(princ txt)
      (if (not (wcmatch (vla-get-name Blk) "`*T*")) ;_exclude table
      (vlax-for	Obj Blk
	(setq count (1+ count))
	(if (zerop(rem count 10))(grtext -1 (strcat txt " : " (itoa count))))
        (setq CountField (ClearFieldInThisObject DOC Obj CountField))
      ) ;_ end of vlax-for
        )
      )
    ) ;_ end of if
  ) ;_ end of vlax-for
 (vl-cmdf "_redrawall")
 CountField 
)
(setq Doc (vla-get-activedocument (vlax-get-acad-object)))
(bg:layer-status-save)(vla-startundomark Doc)
 (if (or (not Ask )
	 (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
	   (bg:msg-yes-no "Внимание"
	     "Все поля будут преобразованы в текст !!!\nПродолжить?"
	     )
	   (bg:msg-yes-no "Attension"
	     "All fields will be transformed to the text!!!\nto Continue?"
	     )
	   )
	 )
 (progn
   (princ "\nConverting Field in ")
   (princ (ClearFieldInAllObjects Doc))
   (princ " text's")
   )
 )
(bg:layer-status-restore)(vla-endundomark Doc)
(command "_.Regenall")  
(princ)
)
(defun ClearFieldInThisObject ( DOC Obj CountField / att txtstr mtList )
    (cond
        ((and (vlax-write-enabled-p Obj)
		 (= (vla-get-ObjectName obj) "AcDbBlockReference")
		 (= (vla-get-HasAttributes obj) :vlax-true)
	    ) ;_ end of and
	  (foreach att 	(append (vlax-invoke obj 'Getattributes)
                                (vlax-invoke obj 'Getconstantattributes)
                                )
            
            ;;;(setq txtstr (vla-get-Textstring att)) ;_Comment VVA 2011-12-09
            (setq txtstr (bg:get-TextString (vlax-vla-object->ename att)));_Add VVA 2011-12-09
	    (vla-put-Textstring att "")
	    (vla-put-Textstring att txtstr)
            (setq Ret t)
	    (setq CountField (1+ CountField))
	  ) ;_ end of foreach
	)
	((and (vlax-write-enabled-p Obj)
		 (vlax-property-available-p Obj 'TextString)
	    ) ;_ end of and
;;;	    (setq txtstr (vla-get-Textstring Obj)) ;_Comment VVA 2011-12-09
            (setq txtstr (bg:get-TextString (vlax-vla-object->ename Obj)));_Add VVA 2011-12-09
            (vl-catch-all-apply '(lambda ()(vla-put-Textstring Obj "")(vla-put-Textstring Obj txtstr))) ;;;VVA 07/10/2014
	    (setq CountField (1+ CountField))
	)
        ((and (vlax-write-enabled-p Obj) ;_Table
              (eq (vla-get-ObjectName Obj) "AcDbTable")
              )
         (and (vlax-property-available-p Obj 'RegenerateTableSuppressed)
                (vla-put-RegenerateTableSuppressed Obj :vlax-true)
              )
         (setq mtlist nil)
         (vlax-for item (vla-item(vla-get-blocks DOC)(cdr(assoc 2 (entget(vlax-vla-object->ename obj)))))
           (if (and (vlax-write-enabled-p item)
		 (vlax-property-available-p item 'TextString)
	    ) ;_ end of and
             (progn
              ;; (setq ii (cons item ii))
               (setq mtlist
                      (cons
                        (list
;;;                          (vla-get-Handle item)
                        (mip_MTEXT_Unformat (vla-get-Textstring item))
                        (bg:get-TextString (vlax-vla-object->ename item))
                        )
                        mtlist
                        )
                     )
               )
            )
           )
         (VL-CATCH-ALL-APPLY 
         '(lambda (col row / i j tmp)
            (setq j '-1 )
            (repeat row
              (setq j (1+ j) i '-1)
              (repeat col
                (setq i (1+ i))
                (if (and (= (vla-GetCellType Obj j i) acTextCell)(not(zerop(vla-GetFieldId obj j i)))) ;_Add VVA 2014-11-06
                  (progn
	                (if (setq tmp (assoc (mip_MTEXT_Unformat(vla-GetText Obj j i)) mtlist))
	                  (progn
	                    ;_(vla-SetText Obj j i "") ;;;_Change VVA 2014-10-30
	                    (vla-SetText Obj j i (cadr tmp))
	                    )
	                  )
                )
                  )
                (setq CountField (1+ CountField))
                )
              )
            )
         (list
           (vla-get-Columns Obj)
           (vla-get-Rows Obj)
           )
           )
         (and (vlax-property-available-p Obj 'RegenerateTableSuppressed)
                (vla-put-RegenerateTableSuppressed Obj :vlax-false)
              )
      (vla-RecomputeTableBlock Obj :vlax-true)
         )
        ((and (vlax-write-enabled-p Obj) ;_Dimension
              (wcmatch (strcase(vla-get-ObjectName Obj)) "*DIMENSION*")
              (not(equal(setq txtstr(vla-get-TextOverride Obj)) ""))
              )
         (vlax-for item (vla-item(vla-get-blocks DOC)(cdr(assoc 2 (entget(vlax-vla-object->ename obj)))))
           (if (and (vlax-write-enabled-p item)
		 (vlax-property-available-p item 'TextString)
	    ) ;_ end of and
             (progn
               (setq txtstr (bg:get-TextString (vlax-vla-object->ename item)))
               )
            )
           )
         (vla-put-TextOverride Obj "")
         (vla-put-TextOverride Obj txtstr)
         (setq CountField (1+ CountField))
         )
        (t nil)
        )
    CountField
    )


(defun C:APPDEL ( / nb nlst lst Rdn adoc )
  ;;;Mip Util mip-del-Rd
  (defun del-XDATA ( en Rdn / elist sub)
   (setq elist (entget en (list "*")) sub nil)
   (foreach i (cdr (assoc -3 elist))
     (if (not(wcmatch (strcase(car i))(strcase Rdn)))
       (setq sub (append sub (list i)))
       (setq sub (append sub (list(list (car i)))))
       ))
  (setq sub (cons -3 sub)
        elist (subst sub (assoc -3 elist) elist) 
        elist (entmod elist))
    )
  (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
;_Получаем список РД Примитивов
  (if (setq nb (ssget "_X" '((-3 ("*")))))
    (progn
      (setq nlst nil lst (vl-remove-if 'listp (mapcar 'cadr (ssnamex nb))))
      (mapcar '(lambda (e1)
	     (mapcar '(lambda(rdn)
			(if (not(member rdn nlst))
			  (setq nlst(append nlst (list rdn)))))
                 (mapcar 'car (car(bg:massoc -3 (entget e1 '("*"))))))
	     )
	  lst)
      (setq nlst(vl-remove-if '(lambda(x)(wcmatch (strcase x) "ACAD*,ACDB*")) nlst))
      (setq Rdn (apply 'strcat (mapcar '(lambda(x)(strcat x ",")) nlst)))
      (mapcar '(lambda(x)(del-XDATA x Rdn)) lst)
      )
    )
   (princ "\nУдаленные РД:")(mapcar 'print nlst)
 (princ))
 
 ;Взорвать (расчленить) МН-БЛОК
;http://dwg.ru/f/showthread.php?t=11502
(defun C:UX (/ adoc blks u1 n obj objlist uname bname *error* pbname ss)
  (defun *error* (msg)
    (princ msg)
    (bg:layer-status-restore)
    (vla-endundomark adoc)
    (princ)
  ) ;_ end of defun
  (defun _copy_unnamed ()
    (setq objlist nil
          u1      (vla-item blks bname)
          n       1
    ) ;_ end of setq
    (vlax-for obj u1
      (grtext -1 (strcat "Обрабатываю примитив " (itoa n)))
      (setq objlist (cons obj objlist))
      (setq n (1+ n))
    ) ;_ end of vlax-for
    (setq n (vla-get-insertionpoint obj))

    (grtext -1 "Копирую примитивы начало ")
    (mapcar '(lambda (item)
               (vla-move item (vlax-3d-point '(0 0 0)) n)
             ) ;_ end of lambda
            (vlax-safearray->list
              (vlax-variant-value
                (vla-copyobjects
                  (vla-get-activedocument (vlax-get-acad-object))
                  (vlax-make-variant
                    (vlax-safearray-fill
                      (vlax-make-safearray
                        vlax-vbobject
                        (cons 0 (1- (length objlist)))
                      ) ;_ end of vlax-make-safearray
                      objlist
                    ) ;_ end of vlax-safearray-fill
                  ) ;_ end of vlax-make-variant
                                                  ;(vla-get-ModelSpace adoc)
                  (vla-get-block
                    (vla-get-activelayout
                      (vla-get-activedocument (vlax-get-acad-object))
                    ) ;_ end of vla-get-ActiveLayout
                  ) ;_ end of vla-get-block
                ) ;_ end of vla-copyobjects
              ) ;_ end of vlax-variant-value
            ) ;_ end of vlax-safearray->list

    ) ;_ end of mapcar
    (grtext -1 "Копирую примитивы конец ")
    (entdel uname)
  ) ;_ end of defun
  (vl-load-com)
  (bg:layer-status-save)
  (setq adoc (vla-get-activedocument (vlax-get-acad-object))
        blks (vla-get-blocks adoc)
  ) ;_ end of setq
  (vla-startundomark adoc)
  (if (and (setq uname (car (entsel "\nВыбери блок")))
           (setq bname  (cdr (assoc 2 (entget uname)))
                 pbname bname
           ) ;_ end of setq
           (wcmatch bname "`*U*,`*E*")
           (setq obj (vlax-ename->vla-object uname))
           (or (and (vlax-property-available-p obj "columns")
                    (vlax-property-available-p obj "rows")
                    (= (vla-get-columns obj) 1)
                    (= (vla-get-rows obj) 1)
               ) ;_ end of and
               (and (not (vlax-property-available-p obj "columns"))
                    (not (vlax-property-available-p obj "rows"))
               ) ;_ end of and
           ) ;_ end of or
      ) ;_ end of and
    (progn
      (_copy_unnamed)

      (vl-catch-all-apply
        '(lambda ()
           (if
             (and
               (setq uname (entlast))
               (setq bname (cdr (assoc 2 (entget uname))))
               (wcmatch bname "`*U*")
               (setq obj (vlax-ename->vla-object uname))
               (or (and (vlax-property-available-p obj "columns")
                        (vlax-property-available-p obj "rows")
                        (= (vla-get-columns obj) 1)
                        (= (vla-get-rows obj) 1)
                   ) ;_ end of and
                   (and (not (vlax-property-available-p obj "columns"))
                        (not (vlax-property-available-p obj "rows"))
                   ) ;_ end of and
               ) ;_ end of or
               (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                 (bg:msg-yes-no
                   "Внимание"
                   (strcat
                     "Похоже, сущестует вложенность анонимных блоков!"
                     "\nПредыдущее имя "
                     pbname
                     "\nТекущее имя "
                     bname
                     "\nПродолжить взрывать анонимные блоки?"
                    ) ;_ end of strcat
                 ) ;_ end of bg:msg-yes-no
                 (bg:msg-yes-no
                   "Attension"
                   (strcat
                     "There nested anonymous blocks!"
                     "\nLast name "
                     pbname
                     "\nCurrent name "
                     bname
                     "\nContinue to explode anonymous blocks?"
                    ) ;_ end of strcat
                 ) ;_ end of bg:msg-yes-no
               ) ;_ end of if
             ) ;_ end of and
             (progn
              (while
                (and
                  (setq uname (entlast))
                  (setq bname (cdr (assoc 2 (entget uname))))
                  (wcmatch bname "`*U*")
                  (not (eq pbname bname))
                  (setq obj (vlax-ename->vla-object uname))
                  (or (and (vlax-property-available-p obj "columns")
                           (vlax-property-available-p obj "rows")
                           (= (vla-get-columns obj) 1)
                           (= (vla-get-rows obj) 1)
                      ) ;_ end of and
                      (and (not (vlax-property-available-p obj "columns"))
                           (not (vlax-property-available-p obj "rows"))
                      ) ;_ end of and
                  ) ;_ end of or
;;;(if (= (getvar "DWGCODEPAGE") "ANSI_1251")
;;;                 (bg:msg-yes-no
;;;                   "Внимание"
;;;                   (strcat
;;;                     "Похоже, сущестует вложенность анонимных блоков!"
;;;                     "\nПредыдущее имя "
;;;                     pbname
;;;                     "\nТекущее имя "
;;;                     bname
;;;                     "\nПродолжить взрывать анонимные блоки?"
;;;                    ) ;_ end of strcat
;;;                 ) ;_ end of bg:msg-yes-no
;;;                 (bg:msg-yes-no
;;;                   "Attension"
;;;                   (strcat
;;;                     "Похоже, сущестует вложенность анонимных блоков!"
;;;                     "\nПредыдущее имя "
;;;                     pbname
;;;                     "\nТекущее имя "
;;;                     bname
;;;                     "\nПродолжить взрывать анонимные блоки?"
;;;                    ) ;_ end of strcat
;;;                 ) ;_ end of bg:msg-yes-no
;;;               )
                  
                ) ;_ end of and
                (vla-endundomark adoc)
                (setq pbname bname)
                 (_copy_unnamed)
                (vla-startundomark adoc)
              ) ;_ end of while
              (while (> (getvar "CMDACTIVE") 0) (command))
              (if (setq ss (ssget "_x" (list (cons 0 "WIPEOUT")(cons 410 (getvar "ctab")))))
                (command "_draworder" ss "" "_b")
                )
              )
           ) ;_ end of if
         ) ;_ end of lambda
      ) ;_ end of VL-CATCH-ALL-APPLY
    ) ;_ end of progn
  ) ;_ end of if
  (bg:layer-status-restore)
  (vla-endundomark adoc)
  (princ)
) ;_ end of defun
(defun bg:objectidtoobject (obj id)
;;; Код ниже закоментирован
;;; Связано стем, что в 2015 Автокаде удалены методы objectidtoobject32
;;; Читать полностью http://help.autodesk.com/view/ACD/2015/ENU/?guid=GUID-6FEDBCCA-91D0-4782-AE5A-49BD4384FD8C
;;
  
;;;  (if (> (vl-string-search "x64" (getvar "platform")) 0)
;;;    (if (vlax-method-applicable-p obj 'objectidtoobject32)(vla-objectidtoobject32 obj id)(vla-objectidtoobject   obj id))
;;;    (vla-objectidtoobject obj id)
;;;    ) ;_ end of if
   (if (vlax-method-applicable-p obj 'objectidtoobject32)
     (vla-objectidtoobject32 obj id)
     (vla-objectidtoobject   obj id)
     )
  ) ;_ end of defun
(defun C:M2B ( / adoc blks u1 n obj objlist uname bname unnamed_block cpo tmp_blk ss lst)
;;;Convert Minsert block To Block
;;; Posted Vladimir Azarko (VVA)
;;; http://forum.dwg.ru/showthread.php?t=11502&page=3(vl-load-com) 
(setq adoc (vla-get-activedocument (vlax-get-acad-object)) 
        blks (vla-get-blocks adoc) 
        ) ;_ end of setq
  (vla-AuditInfo adoc :vlax-true)
  (vla-startundomark adoc)
  (if (setq ss (ssget "_:L" '((0 . "INSERT")
			      (-4 . "<OR")
			      (-4 . ">")(70 . 1)
			      (-4 . ">")(71 . 1)
			      (-4 . "OR>")
			      )))
(progn
  (repeat (setq n (sslength ss)) ;_ end setq
         (setq lst (cons (ssname ss (setq n (1- n))) lst))
         ) ;_ end
  (setq ss nil n 0)
  (foreach uname lst
    (grtext -1 (strcat "Working " (itoa (setq n (1+ n)))))
  (setq bname (cdr(assoc 2 (entget uname))))  
  (setq u1 (vla-item blks bname))
  (setq obj (vlax-ename->vla-object uname) objlist nil)  
  (vlax-for item u1 (setq objlist (cons item objlist)))
 (setq unnamed_block (vla-add (vla-get-blocks adoc)(vlax-3d-point '(0 0 0)) "*U"))
 (setq cpo (vla-copyobjects adoc
                 (vlax-make-variant 
                   (vlax-safearray-fill 
                     (vlax-make-safearray 
                       vlax-vbobject 
                       (cons 0 (1- (length objlist))) 
                     ) ;_ end of vlax-make-safearray 
                     objlist 
                   ) ;_ end of vlax-safearray-fill 
                 ) ;_ end of vlax-make-variant 
                ;(vla-get-ModelSpace adoc)
		 unnamed_block
               )
      )
  (setq tmp_blk (vla-insertblock
		  (bg:objectidtoobject adoc (vla-get-ownerid obj))
		  (vla-get-InsertionPoint obj)
		  (vla-get-name unnamed_block)
		  (vla-get-xscalefactor obj)(vla-get-yscalefactor obj)
	       (vla-get-zscalefactor obj)
		  ;(vla-get-rotation obj)
		  0
		  )
	)
  (setq cpo (vla-ArrayRectangular tmp_blk (vla-get-rows obj)(vla-get-columns obj) 1 (vla-get-RowSpacing obj)(vla-get-ColumnSpacing obj) 0))
  (setq cpo (vlax-safearray->list(vlax-variant-value cpo)))
  (setq cpo (cons tmp_blk cpo))
(foreach item cpo
	(vla-rotate item (vla-get-InsertionPoint obj) (vla-get-rotation obj))
	)
  (entdel uname)
    )
  (princ "Converting ")(princ n)(princ " minsert blocks")
  ) 
  ) 
  (vla-endundomark adoc)
(vl-cmdf "_.Redraw")
(princ)
)
(defun C:U2B (/        adoc     blks     u1       n        obj
              objlist  uname    bname    *error*  bnameNew tmp_blk
             )
  ;;; Unnamed to Block
  (defun *error* (msg)
    (princ msg)
    (bg:layer-status-restore)
    (vla-endundomark adoc)
    (princ)
  ) ;_ end of defun
  (vl-load-com)
  (bg:layer-status-save)
  (setq adoc (vla-get-activedocument (vlax-get-acad-object))
        blks (vla-get-blocks adoc)
  ) ;_ end of setq
  (vla-startundomark adoc)
  (if
    (and (setq uname (car (entsel (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\nВыбери блок: " "\nSelect block: "))))
         (wcmatch (setq bname (cdr (assoc 2 (entget uname)))) "`*U*,`*X*,`*E*")
         (setq obj (vlax-ename->vla-object uname))
         (or (and (vlax-property-available-p obj "columns")
                  (vlax-property-available-p obj "rows")
                  (= (vla-get-columns obj) 1)
                  (= (vla-get-rows obj) 1)
             ) ;_ end of and
             (and (not (vlax-property-available-p obj "columns"))
                  (not (vlax-property-available-p obj "rows"))
             ) ;_ end of and
         ) ;_ end of or
         (setq bnameNew (getstring (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\nИмя блока: " "\nNew block name: ")))
         (or
         (while (or (not (snvalid bnameNew))
                    (member (strcase bnameNew) (tablelist "BLOCK"))
                ) ;_ end of or
           (alert (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\nНедопустимое имя блока" "Incorrect block name"))
           (setq bnameNew (getstring (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\nИмя блока: " "\nNew block name: ")))
         ) ;_ end of while
         t)
    ) ;_ end of and
     (progn
       (setq unnamed_block
              (vla-add (vla-get-blocks adoc)
                       (vlax-3d-point '(0 0 0))
                       bnameNew
              ) ;_ end of vla-add
       ) ;_ end of setq
       (setq u1 (vla-item blks bname)
             n  1
       ) ;_ end of setq
       (vlax-for item u1
         (grtext -1 (strcat "Working ... item " (itoa n)))
         (setq objlist (cons item objlist))
         (setq n (1+ n))
       ) ;_ end of vlax-for
       (setq n (vlax-3d-point(trans(vlax-safearray->list(vlax-variant-value(vla-get-insertionpoint obj))) 0 uname)))
  ;;;     (setq n (vlax-3d-point(trans (vlax-safearray->list(vlax-variant-value n)) uname 0)))
       (grtext -1 "Coping item. Begin ")
       (vla-copyobjects
                     adoc
                     (vlax-make-variant
                       (vlax-safearray-fill
                         (vlax-make-safearray
                           vlax-vbobject
                           (cons 0 (1- (length objlist)))
                         ) ;_ end of vlax-make-safearray
                         objlist
                       ) ;_ end of vlax-safearray-fill
                     ) ;_ end of vlax-make-variant
                                                  ;(vla-get-ModelSpace adoc)
                     unnamed_block
                   ) ;_ end of
       (grtext -1 "Coping item. End ")
       (setq
         tmp_blk (vla-insertblock
                   (bg:objectidtoobject adoc (vla-get-ownerid obj))
                                                  ;(vla-get-InsertionPoint obj)
                   n
                   (vla-get-name unnamed_block)
                   (vla-get-xscalefactor obj)
                   (vla-get-yscalefactor obj)
                   (vla-get-zscalefactor obj)
                   (vla-get-rotation obj)
                 ) ;_ end of vla-insertblock
       ) ;_ end of setq
(mapcar
	   '(lambda (x y) (vlax-put-property tmp_blk x y))
	   '(Linetype LineWeight Color Layer)
	   (mapcar
	     '(lambda (x)
		(vlax-get-property obj x))
	     '(Linetype LineWeight Color Layer)))
       (vla-put-Normal tmp_blk (vla-get-Normal obj))
       (entdel uname)
     ) ;_ end of progn
  ) ;_ end of if
  (bg:layer-status-restore)
  (vla-endundomark adoc)
  (princ)
) ;_ end of defun

(defun C:U2BM (/   ss    adoc     blks pat    u1 i      n        obj
              objlist  uname    bname    *error*  bnameNew tmp_blk
             )
  ;;; Unnamed to Block Multiple
  (defun *error* (msg)
    (princ msg)
    (bg:layer-status-restore)
    (vla-endundomark adoc)
    (princ)
  ) ;_ end of defun
  (vl-load-com)
  (bg:layer-status-save)
  (setq adoc (vla-get-activedocument (vlax-get-acad-object))
        blks (vla-get-blocks adoc)
  ) ;_ end of setq
  (vla-startundomark adoc)
  (setq pat "U2B-")
 (if (setq ss (ssget '((0 . "INSERT"))))
   (repeat (setq i (sslength ss))
     (setq uname (ssname ss (setq i (1- i))))
  (if
    (and (wcmatch (setq bname (cdr (assoc 2 (entget uname)))) "`*U*,`*X*")
         (setq obj (vlax-ename->vla-object uname))
         (eq
         (cond
           ((and (vlax-property-available-p obj 'isdynamicblock)
                 (= (vla-get-isdynamicblock obj) :vlax-true)
                 ) ;_ end of and
            (vla-get-effectivename obj)
            )
           (t (vla-get-name obj))
           )
         bname
         )
         (or (and (vlax-property-available-p obj "columns")
                  (vlax-property-available-p obj "rows")
                  (= (vla-get-columns (vlax-ename->vla-object uname)) 1)
                  (= (vla-get-rows (vlax-ename->vla-object uname)) 1)
             ) ;_ end of and
             (and (not (vlax-property-available-p obj "columns"))
                  (not (vlax-property-available-p obj "rows"))
             ) ;_ end of and
         ) ;_ end of or
         (setq n 0 bnameNew (strcat pat (substr bname 2)))
         (or
         (while (or (not (snvalid bnameNew))
                    (member (strcase bnameNew) (tablelist "BLOCK"))
                ) ;_ end of or
              (setq bnameNew (strcat pat (substr bname 2) "-" (itoa (setq n (1+ n)))))
         ) ;_ end of while
         t)
    ) ;_ end of and
     (progn
       (setq unnamed_block
              (vla-add (vla-get-blocks adoc)
                       (vlax-3d-point '(0 0 0))
                       bnameNew
              ) ;_ end of vla-add
       ) ;_ end of setq
       (setq u1 (vla-item blks bname)
             n  1
             objlist nil
       ) ;_ end of setq
       (vlax-for item u1
         (grtext -1 (strcat "Working ... item " (itoa n)))
         (setq objlist (cons item objlist))
         (setq n (1+ n))
       ) ;_ end of vlax-for
   ;;;    (setq n (vla-get-insertionpoint (vlax-ename->vla-object uname)))
      (setq n (vlax-3d-point(trans(vlax-safearray->list(vlax-variant-value(vla-get-insertionpoint obj))) 0 uname)))
       (grtext -1 "Coping item. Begin ")
       (vla-copyobjects
                     adoc
                     (vlax-make-variant
                       (vlax-safearray-fill
                         (vlax-make-safearray
                           vlax-vbobject
                           (cons 0 (1- (length objlist)))
                         ) ;_ end of vlax-make-safearray
                         objlist
                       ) ;_ end of vlax-safearray-fill
                     ) ;_ end of vlax-make-variant
                                                  ;(vla-get-ModelSpace adoc)
                     unnamed_block
                   ) ;_ end of 
       (grtext -1 "Coping item. End ")
       (setq
         tmp_blk (vla-insertblock
                   (bg:objectidtoobject adoc (vla-get-ownerid obj))
                                                  ;(vla-get-InsertionPoint obj)
                   n
                   (vla-get-name unnamed_block)
                   (vla-get-xscalefactor obj)
                   (vla-get-yscalefactor obj)
                   (vla-get-zscalefactor obj)
                   (vla-get-rotation obj)
                 ) ;_ end of vla-insertblock
       ) ;_ end of setq
(mapcar
	   '(lambda (x y) (vlax-put-property tmp_blk x y))
	   '(Linetype LineWeight Color Layer)
	   (mapcar
	     '(lambda (x)
		(vlax-get-property obj x))
	     '(Linetype LineWeight Color Layer)))
      (vla-put-Normal tmp_blk (vla-get-Normal obj))
       (entdel uname)
     ) ;_ end of progn
  ) ;_ end of if
     )
   )
  (bg:layer-status-restore)
  (vla-endundomark adoc)
  (princ)
) ;_ end of defun


  ;;;Convert Minsert block To Block
  ;;;http://dwg.ru/f/showthread.php?t=11502&page=3
(defun C:M2U ( / adoc blks u1 n obj objlist uname bname unnamed_block cpo tmp_blk ss lst)
(vl-load-com) 
(setq adoc (vla-get-activedocument (vlax-get-acad-object)) 
        blks (vla-get-blocks adoc) 
        ) ;_ end of setq 
  (vla-startundomark adoc)
  
  (if (setq ss (ssget "_:L" '((0 . "INSERT")(-4 . ">")(70 . 1)(71 . 1))))
(progn
  (repeat (setq n (sslength ss)) ;_ end setq
         (setq lst (cons (ssname ss (setq n (1- n))) lst))
         ) ;_ end
  (setq ss nil n 0)
  (foreach uname lst
    (grtext -1 (strcat "Working " (itoa (setq n (1+ n)))))
  (setq bname (cdr(assoc 2 (entget uname))))  
  (setq u1 (vla-item blks bname))
  (setq obj (vlax-ename->vla-object uname) objlist nil)  
  (vlax-for item u1 (setq objlist (cons item objlist)))
 (setq unnamed_block (vla-add (vla-get-blocks adoc)(vlax-3d-point '(0 0 0)) "*U"))
 (setq cpo (vla-copyobjects adoc
                 (vlax-make-variant 
                   (vlax-safearray-fill 
                     (vlax-make-safearray 
                       vlax-vbobject 
                       (cons 0 (1- (length objlist))) 
                     ) ;_ end of vlax-make-safearray 
                     objlist 
                   ) ;_ end of vlax-safearray-fill 
                 ) ;_ end of vlax-make-variant 
                ;(vla-get-ModelSpace adoc)
		 unnamed_block
               )
      )
  (setq tmp_blk (vla-insertblock
		  (bg:objectidtoobject adoc (vla-get-ownerid obj))
		  (vla-get-InsertionPoint obj)
		  (vla-get-name unnamed_block)
		  (vla-get-xscalefactor obj)(vla-get-yscalefactor obj)
	       (vla-get-zscalefactor obj)
		  ;(vla-get-rotation obj)
		  0
		  )
	)
  (setq cpo (vla-ArrayRectangular tmp_blk (vla-get-rows obj)(vla-get-columns obj) 1 (vla-get-RowSpacing obj)(vla-get-ColumnSpacing obj) 0))
  (setq cpo (vlax-safearray->list(vlax-variant-value cpo)))
  (setq cpo (cons tmp_blk cpo))
(foreach item cpo
	(vla-rotate item (vla-get-InsertionPoint obj) (vla-get-rotation obj))
	)
  (entdel uname)
    )
  (princ "Converting ")(princ n)(princ " minsert blocks")
  ) 
  ) 
  (vla-endundomark adoc)
(vl-cmdf "_.Redraw")
(princ)
)
(defun C:BGCOLORXREF (/ doc col xreflist)
  (vl-load-com)
;;;  (alert
;;;    "\This lisp change color xref\nONLY ON A CURRENT SESSION"
;;;  ) ;_ end of alert
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark doc)
  (bg:layer-status-save)
   (vlax-for item (vla-get-Blocks doc)
      (if (= (vla-get-IsXref item) :vlax-true)
         (setq xreflist (cons (vla-get-name item) xreflist))
        )
     )
  (if (and xreflist
           (setq xreflist (_dwgru-get-user-dcl "Выберите ссылки " (acad_strlsort xreflist) t))
           (setq col (acad_colordlg 7 t))
           )
    (ChangeXrefAllObjectsColor doc col xreflist) ;_ col — color number
  ) ;_ end of if
  (bg:layer-status-restore)
  (vla-endundomark doc)
  (princ)
) ;_ end of defun
;;; ************************************************************************
;;; * Библиотека DWGruLispLib Copyright ©2008  DWGru Programmers Group
;;; *
;;; * _dwgru-get-user-dcl (Кандидат)
;;; *
;;; * Запрос значения у пользователя через диалоговое окно
;;; *
;;; *
;;; * 26/01/2008 Версия 0002. Редакция Владимир Азарко (VVA)
;;;              - Выход по двойному клику, если запрещен множественный выбор (multi-nil)
;;;              - Обработка нескольких колонок
;;; * 21/01/2008 Версия 0001. Редакция Владимир Азарко (VVA)
;;; ************************************************************************


;;; ************************************************************************
;;; * Library DWGruLispLib Copyright © 2008 DWGru Programmers Group
;;; *
;;; * _dwgru-get-user-dcl (Candidate)
;;; *
;;; * Inquiry of value at the user through a dialogue window
;;; *
;;; *
;;; * 26/01/2008 Version 0002. Edition Vladimir Azarko (VVA)
;;; - the Output on double a clique if the plural choice (multi-nil) is forbidden
;;; - Processing of several columns
;;; * 21/01/2008 Version 0001. Edition Vladimir Azarko (VVA)


(defun _DWGRU-GET-USER-DCL (ZAGL        INFO-LIST   MULTI
                            /           FL          RET
                            DCL_ID      MAXROW      MAX_COUNT_COL
                            COUNT_COL   I           LISTBOX_HEIGHT
                            LST         _LOC_FINISH _LOC_CLEAR
                            NCOL tmp
                           )
;| 
* ENGLISH
* Inquiry of value at the user through a dialogue window
* Dialogue is formed to "strike"
* the Quantity of lines on page without scrolling is set by variable MAXROW.
* It is necessary to remember, that number MAXROW increases on 3.
* the Maximum quantity of columns is set by variable MAX_COUNT_COL
* It is published
     http://dwg.ru/f/showthread.php?p=203746#post203746
* Parameters of a call:
    zagl - heading of a window [String]
    info-list - the list of line values [List of String]
    multi - t - the plural choice is resolved, nil-is not present
      
* Returns:
 The list of the chosen lines or nil - a cancelling
* the Example
 (_dwgru-get-user-dcl " Specify a variant " ' ("First" "Second" "Third") nil); _-> ("First") 
 (_dwgru-get-user-dcl " Specify a variant " ' ("First" "Second" "Third") t); _-> ("First"  "Second ")
 (_dwgru-get-user-dcl " Specify a variant "
   (progn (setq i 0 lst nil) (repeat 205 (setq lst (cons (strcat "Значение-" (itoa (setq i (1 + i)))) lst))) (reverse lst)) nil)
 (_dwgru-get-user-dcl " Specify a variant, using CTRL and SHIFT for a choice "
   (progn (setq i 0 lst nil) (repeat 205 (setq lst (cons (strcat "Значение-" (itoa (setq i (1 + i)))) lst))) (reverse lst)) t)
|;
;|
* RUS						   
* Запрос значения у пользователя через диалоговое окно
* Диалог формируется "налету"
* Количество строк на страницу без скроллинга задается переменной MAXROW.
* Необходимо помнить, что число MAXROW увеличивается на 3.
* Максимальное количество колонок задается переменной MAX_COUNT_COL
* Опубликована
     http://dwg.ru/f/showthread.php?p=203746#post203746
* Параметры вызова:
    zagl - заголовок окна [String]
    info-list - список строковых значений [List of String]
    multi - t - разрешен множественный выбор, nil- нет
    
* Возвращает:
 Список выбранных строк или nil - отмена
* Пример
 (_dwgru-get-user-dcl "Укажите вариант" '("Первый" "Второй" "Третий") nil) ;_->("Первый") 
 (_dwgru-get-user-dcl "Укажите вариант" '("Первый" "Второй" "Третий") t) ;_->("Первый" "Второй")
 (_dwgru-get-user-dcl "Укажите вариант"
   (progn (setq i 0 lst nil)(repeat 205 (setq lst (cons (strcat "Значение-" (itoa (setq i (1+ i)))) lst)))(reverse lst)) nil)
 (_dwgru-get-user-dcl "Укажите вариант, используя CTRL и SHIFT для выбора"
   (progn (setq i 0 lst nil)(repeat 205 (setq lst (cons (strcat "Значение-" (itoa (setq i (1+ i)))) lst)))(reverse lst)) t)
|;
 ;_ ===== КОНСТАНТЫ ============

  (setq MAXROW 40) ;_макc. кол-во строк без скроллинга (К нему дальше добавится еще 3 строчки)
                   ;_  max lines without scrolling (To it 3 more lines further will be added)
  (setq MAX_COUNT_COL 5) ;_максимальное количество колонок
                         ;_ ; _ a maximum quantity of columns
;;============== Локальные фунцкции START==================
;;============== Local functions START========================

  (defun _LOC_FINISH ()
    (setq I   0
          RET NIL
    ) ;_ end ofsetq
    (repeat COUNT_COL
      (setq I (1+ I))
      (setq RET (cons (cons I (get_tile (strcat "info" (itoa I)))) RET))
    ) ;_ end ofrepeat
    (setq RET (reverse RET))
    (done_dialog 1)
  ) ;_ end ofdefun
  (defun _LOC_ERR-TILE (what)
    ;;;what - string or nil
    (if what
      (set_tile "error" what)
    (if MULTI
        (set_tile "error"
                  (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                    "Используйте CTRL и SHIFT для выбора"
                    "Use CTRL and SHIFT for a choicet"
                  ) ;_ end ofif
        ) ;_ end ofset_tile
        (set_tile "error"
                  (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                    "Можно выбирать двойным щелчком"
                    "It is possible to choose double click"
                  ) ;_ end ofif
        ) ;_ end ofset_tile
      ) ;_ end ofif
      )
    )
  (defun _LOC_CLEAR (NOMER)
    (setq I 0)
    (repeat COUNT_COL
      (setq I (1+ I))
      (if (/= I NOMER)
        (progn
          (start_list (strcat "info" (itoa I)))
          (mapcar 'add_list (nth (1- I) LST))
          (end_list)
        ) ;_ end ofprogn
      ) ;_ end ofif
    ) ;_ end ofrepeat
  ) ;_ end ofdefun

;;;==================== Локальные фунцкции END ==================================
;;;==================== Local functions END ==================================

;;;==================== MAIN PART ===============================================

  (if (null ZAGL)
    (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
      (setq ZAGL "Выбор")
      (setq ZAGL "Select")
    ) ;_ end ofif
  ) ;_ end if
  (if (zerop (rem (length INFO-LIST) MAXROW)) ;_Целое количество столбцов
    (setq COUNT_COL (/ (length INFO-LIST) MAXROW)) ;_Его и оставляем
    (setq COUNT_COL (1+ (fix (/ (length INFO-LIST) MAXROW 1.0)))) ;_Берем ближайшее целое
  ) ;_ end ofif
  (if (> COUNT_COL MAX_COUNT_COL)
    (setq COUNT_COL MAX_COUNT_COL)
  ) ;_Ограничиваем max количеством
  (setq LISTBOX_HEIGHT (+ 3 MAXROW)) ;_  добавляем 3 строчки для красоты и для исключения пограничного скроллинга
                                     ;_ We add 3 lines for appearance and for exception boundary scroll
  (if (and (= COUNT_COL 1) (<= (length INFO-LIST) MAXROW))
    (setq LISTBOX_HEIGHT (+ 3 (length INFO-LIST)))
  ) ;_ end ofif
  (setq I 0)
  (setq FL (vl-filename-mktemp "dwgru" NIL ".dcl"))
  (setq RET (open FL "w")
        LST NIL
  ) ;_ end ofsetq
  (mapcar '(lambda (X) (write-line X RET))
          (append (list "dwgru_get_user : dialog { "
                        (strcat "label=\"" ZAGL "\";")
                        ": boxed_row {"
                        (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                          "label = \"Значение\";"
                          "label = \"Value\";"
                        ) ;_ end ofif
                  ) ;_ end oflist
                  (repeat COUNT_COL
                    (setq LST
                           (append
                             LST
                             (list
                               " :list_box {"
                               "alignment=top ;"
                               (if MULTI
                                 "multiple_select = true ;"
                                 "multiple_select = false ;"
                               ) ;_ end ofif
                               (strcat
                               "width="
                               (itoa
                               ((lambda(len)
                                  (setq len
                                  (cond ((and
                                           (< COUNT_COL 3)
                                           (< len 93)
                                           )
                                         len
                                         )
                                        ((and
                                           (> COUNT_COL 2)
                                           (< len 73)
                                           )
                                         len
                                         )
                                        (t 41)
                                        )
                                        )
                                  (if (< len 25) 25 len)
                                  )
                                 (apply 'max (mapcar 'strlen info-list))
                                 )
                               )
                               ";"
                               )
                               (strcat "height= " (itoa LISTBOX_HEIGHT) " ;")
                               "is_tab_stop = false ;"
                               (strcat "key = \"info" (itoa (setq I (1+ I))) "\";}")
                             ) ;_ end oflist
                           ) ;_ end ofappend
                    ) ;_ end ofsetq
                  ) ;_ end ofrepeat
                  (list
                    "}"
                    ":row{"
                    "ok_cancel_err;}}"
                  ) ;_ end oflist
          ) ;_ end of list
  ) ;_ end of mapcar
  (setq RET (close RET))
  (if (and (null (minusp (setq DCL_ID (load_dialog FL))))
           (new_dialog "dwgru_get_user" DCL_ID)
      ) ;_ end and
    (progn
      (setq LST INFO-LIST)
      ((lambda (/ RET1 BUF ITM)

         (repeat (1- COUNT_COL)
           (setq I '-1)
           (while (and (setq ITM (car LST))
                       (< (setq I (1+ I)) MAXROW)
                  ) ;_ end ofand
             (setq BUF (cons ITM BUF)
                   LST (cdr LST)
             ) ;_ end ofsetq
           ) ;_ end ofwhile
           (setq RET1 (cons (reverse BUF) RET1)
                 BUF  NIL
           ) ;_ end ofsetq
         ) ;_ end ofrepeat
         (setq RET RET1)
       ) ;_ end oflambda
      )
      (if LST
        (setq RET (cons LST RET))
      ) ;_ end ofif
      (setq LST (reverse RET))
      (setq I 0)
      (mapcar '(lambda (THIS_LIST)
                 (if (<= (setq I (1+ I)) COUNT_COL)
                   (progn
                     (start_list (strcat "info" (itoa I)))
                     (mapcar 'add_list THIS_LIST)
                     (end_list)
                   ) ;_ end ofprogn
                 ) ;_ end ofif
               ) ;_ end oflambda
              LST
      ) ;_ end ofmapcar

      (set_tile "info1" "0")
      (setq I 0
            NCOL 1
      ) ;_ end ofsetq
      (repeat COUNT_COL
        (action_tile
          (strcat "info" (itoa (setq I (1+ I))))
          (strcat "(progn (if (= $reason 1)(_LOC_ERR-TILE (nth (atoi(get_tile \"info1\")) info-list))(_LOC_ERR-TILE nil)) (setq Ncol "
                  (itoa I)
                  ")(if (not multi)(_loc_clear Ncol))"
                  "(if (and (not multi)(= $reason 4))(_loc_finish)))"
          ) ;_ end ofstrcat
        ) ;_ end ofaction_tile
      ) ;_ end ofrepeat
      (action_tile "cancel" "(done_dialog 0)")
      (action_tile "accept" "(_loc_finish)")
      (_LOC_ERR-TILE nil)
      (if (zerop (start_dialog))
        (setq RET NIL)
        (progn
          (setq
            RET (apply
                  'append
                  (mapcar
                    '(lambda (ITM)
                       (setq THIS_LIST (nth (1- (car ITM)) LST))
                       (mapcar
                         (function (lambda (NUM) (nth NUM THIS_LIST)))
                         (read (strcat "(" (cdr ITM) ")"))
                       ) ;_ end ofmapcar
                     ) ;_ end oflambda
                    RET
                  ) ;_ end ofmapcar
                ) ;_ end ofapply
          ) ;_ end ofsetq

        ) ;_ end ofprogn
      ) ;_ end if
      (unload_dialog DCL_ID)
    ) ;_ end of progn
  ) ;_ end of if
  (vl-file-delete FL)
  RET
) ;_ end ofdefun

(defun ChangeXrefAllObjectsColor (Doc Color xreflist / tmp txtstr txt count TrueColor pat )
  (setq xreflist (mapcar 'strcase xreflist))  
  (setq pat(mapcar '(lambda(x)(strcat x "|*,")) xreflist))
  (setq pat (apply 'strcat pat))

  (vlax-for item (vla-get-Layers doc)
      (if (wcmatch (strcase (vla-get-name item)) pat)
	(vla-put-color item color)
      )
    )
  (vlax-for Blk	(vla-get-Blocks Doc)
    (cond
      (
       (or (and (= (vla-get-IsXref Blk) :vlax-true)
                (member (strcase(vla-get-name Blk)) xreflist)
                )
	   (and	(= (vla-get-IsXref Blk) :vlax-false)
		(wcmatch (strcase(vla-get-name Blk)) pat );_ "*|*"
	   ) ;_ end of and
       ) ;_ end of or
       	(setq count 0 txt (strcat "Changed " (vla-get-name Blk)))
        (grtext -1 txt)
       (vlax-for Obj Blk
      	(setq count (1+ count))
	;;;(if (zerop(rem count 10))(grtext -1 (strcat txt " : " (itoa count))))
         (grtext -1 (strcat txt " : " (itoa count)))
         (if (and
               (vlax-write-enabled-p Obj)
               (vlax-property-available-p Obj 'TrueColor)
               (not(eq(vla-get-ObjectName Obj) "AcDbZombieEntity"))
               )
           (progn
             (setq TrueColor (vla-get-TrueColor obj))
             (vla-put-ColorMethod TrueColor acColorMethodByACI)
             (vla-put-ColorIndex TrueColor Color)
             (VL-CATCH-ALL-APPLY 'vla-put-TrueColor (list obj TrueColor))
             )
           )
	 (if (and (vlax-write-enabled-p Obj)
		  (vlax-property-available-p Obj 'Color)
                  (not(eq(vla-get-ObjectName Obj) "AcDbZombieEntity"))
	     ) ;_ end of and
	   (VL-CATCH-ALL-APPLY 'vla-put-Color (list Obj Color))
	 ) ;_ end of if
	 (if (and (vlax-write-enabled-p Obj)
		 (vlax-property-available-p Obj 'TextString)
	    ) ;_ end of and
	  (progn
;;; >>>--------- comment VVA 2011-04-28            
;;;	    (setq txtstr
;;;		   (if (vlax-method-applicable-p Obj 'FieldCode)
;;;		       (vla-FieldCode Obj)
;;;		       (vlax-get-property Obj 'TextString))
;;;		  )
;;; <<< --------- comment VVA 2011-04-28            

;;; >>>--------- ADD VVA 2011-04-28                        
            (setq txtstr (bg:FieldCode (vlax-vla-object->ename Obj)))
;;; <<<--------- ADD VVA 2011-04-28                                    
	    (setq tmp 0)
	     (while (setq tmp (VL-STRING-SEARCH "\\C" txtstr tmp))
	      (setq txtstr
	      (vl-string-subst
		(strcat (substr txtstr (1+ tmp) 2)(itoa Color) ";")
		(substr txtstr (1+ tmp) (- (1+ (VL-STRING-SEARCH ";" txtstr tmp)) tmp))
		txtstr
		tmp)
		    )
	      (setq tmp (+ tmp 3))
	      )
	    ;;;(vla-put-Textstring Obj txtstr)
            (VL-CATCH-ALL-APPLY 'vla-put-Textstring (list Obj txtstr))
	    )
	) ;_ end of if
	 (if (and (vlax-write-enabled-p Obj)
		  (= (vla-get-ObjectName obj) "AcDbBlockReference")
		  (= (vla-get-HasAttributes obj) :vlax-true)
	     ) ;_ end of and
           (vl-catch-all-apply '(lambda()
	   (foreach att	(vlax-safearray->list
			  (vlax-variant-value (vla-GetAttributes obj))
			) ;_ end of vlax-safearray->list
	     (if (and (vlax-write-enabled-p att)
		      (vlax-property-available-p att 'Color)
		 ) ;_ end of and
	       (VL-CATCH-ALL-APPLY 'vla-put-Color (list att Color)) ;_(vla-put-Color att Color)
	     ) ;_ end of if
	   ) ;_ end of foreach
                                  )
             )
	 ) ;_ end of if
	 (if (and (vlax-write-enabled-p Obj)
		  (wcmatch (vla-get-Objectname Obj) "*Dimension*,AcDb*Leader")
	     ) ;_ end of and
	   (progn
	     (vl-catch-all-apply 'vla-put-ExtensionLineColor (list Obj Color))
	     (vl-catch-all-apply 'vla-put-TextColor (list Obj Color))
	     (vl-catch-all-apply 'vla-put-DimensionLineColor (list Obj Color))
	     (if (vlax-property-available-p Obj 'LeaderLineColor)
	       (progn
		 (setq tmp (vla-getinterfaceobject(vlax-get-acad-object)(strcat "AutoCAD.AcCmColor."
		(substr (getvar "ACADVER") 1 2))))
		 (vla-put-colorindex  tmp  Color)
		 (vl-catch-all-apply 'vla-put-LeaderLineColor (list Obj tmp))
		 )
	       )
	   ) ;_ end of progn
	 ) ;_ end of if
       ) ;_ end of vlax-for
      )
      (t nil)
    ) ;_cond
  ) ;_ end of vlax-for
    ;;;Обработка стилей мультилиний

  (vla-regen Doc acAllViewports)
;;  (vl-cmdf "_regenall")
) ;_ end of defun
(defun bg:hatch-delete-form-Block ( Blk )
  ;;; Blk -vla-object "AcDbBlockTableRecord"
    (if	(= (vla-get-IsXref Blk) :vlax-false)
      (progn
	(if (> (vla-get-count Blk) 100)
	  (bg:progress-init
	    (strcat (vla-get-name Blk) " :")
	    (vla-get-count Blk)
	  ) ;_ end of bg:progress-init
	  (progn
	    (setvar "MODEMACRO" (vla-get-name Blk))
	  ) ;_ end of progn
	) ;_ end of if
	(vlax-for Obj Blk
	  (if (and
                (= (vla-get-ObjectName Obj) "AcDbHatch")
                (vlax-write-enabled-p Obj)
                )
            (vl-catch-all-apply 'vla-delete (list Obj))
	  ) ;_ end of if
	) ;_ end of vlax-for
	(bg:progress-clear)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun
(defun bg:hatch-delete ( adoc IgnoreLockLayer /  *error*)
  ;;; adoc - active document (setq aDOC (vla-get-activedocument (vlax-get-acad-object)))
  ;;; IgnoreLockLayer -t - ignore nil - not
  (defun *error* (msg)
    (setvar "MODEMACRO" "")
    (princ msg)
    (vla-regen aDOC acactiveviewport)
    (bg:progress-clear)
    (bg:layer-status-restore)
    (princ)
  ) ;_ end of defun
  
  ;;; (setq aDOC (vla-get-activedocument (vlax-get-acad-object)))
  (and IgnoreLockLayer
       (bg:layer-status-save)
       )
  (vlax-for Blk (vla-get-Blocks aDOC)
	(bg:hatch-delete-form-Block Blk)
    )
  (bg:layer-status-restore)
    (vla-regen aDOC acActiveViewport)
  (princ)
) ;_ end of defun
(defun C:BGHATCHDEL ()
  (bg:hatch-delete (vla-get-activedocument (vlax-get-acad-object))
    (bg:msg-yes-no
      (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "Удаление Штриховки" "Remove the hatch")
      (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "Удалять штриховку с \nзаблокированных или замороженных слоев?" "Remove hatch from \n locked or frozen layers?")
    )
  )
  )

;;;(defun c:erase-hatch (/ adoc)
;;;  (vla-startundomark (setq adoc (vla-get-activedocument (vlax-get-acad-object))))
;;;  (vlax-for blk_def (vla-get-blocks adoc)
;;;    (if (equal (vla-get-isxref blk_def) :vlax-false)
;;;      (vlax-for ent blk_def
;;;        (if (= (vla-get-objectname ent) "AcDbHatch")
;;;          (vl-catch-all-apply
;;;            (function
;;;              (lambda ()
;;;                (vla-erase ent)
;;;                ) ;_ end of lambda
;;;              ) ;_ end of function
;;;            ) ;_ end of vl-catch-all-apply
;;;          ) ;_ end of if
;;;        ) ;_ end of vlax-for
;;;      ) ;_ end of if
;;;    ) ;_ end of vlax-for
;;;  (vla-endundomark adoc)
;;;  (princ)
;;;  ) ;_ end of defun

(defun bg:Color-to-ACIcolor (/ txt count *error*)
  (defun *error* (msg)
    (princ msg)
    (bg:layer-status-restore)
    (princ)
  ) ;_ end of defun
  (bg:layer-status-save)
  (vlax-for Blk	(vla-get-blocks
		  (vla-get-activedocument (vlax-get-acad-object))
		) ;_ end of vla-get-blocks
    (setq count 0)
    (grtext -1
	    (setq txt
		   (strcat "Inspecting objects: "
			   (vla-get-name Blk)
		   )
	    )
    ) ;_ end of grtext
    (if (or
          (not(wcmatch (vla-get-name Blk) "*|*"))
          (eq (vla-get-isxref Blk) :vlax-false)
          )
      (progn
	(vlax-for Obj Blk
	  (setq count (1+ count))
	  (if (zerop (rem count 10))
	    (grtext -1 (strcat txt " : " (itoa count)))
	  )
	  (if (and (vlax-write-enabled-p Obj)
		   (vlax-property-available-p Obj 'Color)
	      )
	    (vla-put-color Obj (vla-get-color Obj))
	  )
	) ;_ end of vlax-for
      ) ;_ end of progn
      (progn
          (princ "\nSkip=")(princ (vla-get-name Blk))
        )
    ) ;_ end of if
  ) ;_ end of vlax-for
  (vlax-for Lay	(vla-get-layers
		  (vla-get-activedocument (vlax-get-acad-object))
		)
    (vla-put-color Lay (vla-get-color Lay))
  )
  (bg:layer-status-restore)
 )
(defun C:BGRGB2ACI ( )
  (bg:Color-to-ACIcolor)
  (command "_.Regenall")
  (princ)
  )
;|
;;;http://www.cadtutor.net/forum/showthread.php?t=533&page=8
(defun c:blccA ()
;;;blccA - BLock Change Color Area
  (pl:block-colorA)
  (princ)
) ;_ end of defun
;;;get from  Alaspher  http://forum.dwg.ru/showthread.php?t=1036
;;; http://forum.dwg.ru/showpost.php?p=166220&postcount=18
(defun pl:block-colorA (/ adoc blocks color ins lays ss lst)
  (setq adoc   (vla-get-activedocument (vlax-get-acad-object))
        blocks (vla-get-blocks adoc)
        lays   (vla-get-layers adoc)
  ) ;_ end of setq
  (if (and (setq color (acad_colordlg 256))
           (setq ss (ssget '((0 . "INSERT"))))
           (progn
             (repeat (setq ins (sslength ss)) ;_ end setq
               (setq lst (cons (ssname ss (setq ins (1- ins))) lst))
             ) ;_ end repeat
             lst
           ) ;_ end of progn
      ) ;_ end of and
    (progn
      (vla-startundomark adoc)
      (foreach ins lst
        (setq ins (vlax-ename->vla-object ins))
        (if (= (vla-get-objectname ins) "AcDbBlockReference")
          (if (vlax-property-available-p ins 'path)
            (princ "\nThis is external reference! Skip.")
            (_pl:block-color blocks ins color lays)
          ) ;_ end of if
          (princ "\nThis isn't block! Try pick other.")
        ) ;_ end of if
      ) ;_ end of repeat
      (vla-regen adoc acallviewports)
      (vla-endundomark adoc)
    ) ;_ end of progn
  ) ;_ end of if
  (princ)
) ;_ end of defun

(defun _pl:block-color (blocks ins color lays / lay layfrz layloc)
  (vlax-for e (vla-item blocks (vla-get-name ins))
    (setq lay (vla-item lays (vla-get-layer e)))
    (if (= (vla-get-freeze lay) :vlax-true)
      (progn (setq layfrz (cons lay layfrz))
             (vla-put-freeze lay :vlax-false)
      ) ;_ end of progn
    ) ;_ end of if
    (if (= (vla-get-lock lay) :vlax-true)
      (progn (setq layloc (cons lay layloc))
             (vla-put-lock lay :vlax-false)
      ) ;_ end of progn
    ) ;_ end of if
    (vl-catch-all-apply (function vla-put-color) (list e color))
    (if (and (= (vla-get-objectname e) "AcDbBlockReference")
             (not (vlax-property-available-p e 'path))
        ) ;_ end of and
      (_pl:block-color blocks e color lays)
    ) ;_ end of if
    (foreach i layfrz (vla-put-freeze i :vlax-true))
    (foreach i layloc (vla-put-lock i :vlax-true))
  ) ;_ end of vlax-for
) ;_ end of defun

(progn
  (princ
    "\BLCCA - Changes in the color of selected blocks in the area"
  ) ;_ end of princ
  (princ)
) ;_ end of progn

|;
(defun bg:blfixdialog ( / dcl dch all_block_list fix_block_list what opt Express )
  (defun sync1 ( )
    (setq all_block_list(ACAD_STRLSORT
      (vl-remove-if-not '(lambda(x) (or
                                      (and
                                          (bg:bitset opt 2048) ;;;Unnamed
                                          (wcmatch x "`*U*")
                                          )
                                      (snvalid x)
                                      )
                           )
      ((lambda (s / d r)
               (while (setq d (tblnext s (null d)))
                 (setq r (cons (cdr (assoc 2 d)) r))
                 )
               )
               "block"
               )
                           
        )
             )
           )
       (updatelist "allblock" all_block_list)
       (updatelist "fixblock" (setq fix_block_list nil))
       (set_tile "allblock" "0")
    )
  (defun _move_ux ()
    (if (= (get_tile "ux") "0")
      (progn
        (updatelist
          "allblock"
          (vl-remove-if-not 'snvalid all_block_list)
        ) ;_ end of updatelist
        (updatelist
          "fixblock"
          (vl-remove-if-not 'snvalid fix_block_list)
        ) ;_ end of updatelist
      ) ;_ end of progn
      (progn
        (updatelist "allblock" all_block_list)
        (updatelist "fixblock" fix_block_list)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun
  (defun sync2 ( )
    (setq fix_block_list(ACAD_STRLSORT
      (vl-remove-if-not '(lambda(x) (or
                                      (and
                                          (bg:bitset opt 2048) ;;;Unnamed
                                          (wcmatch x "`*U*")
                                          )
                                      (snvalid x)
                                      )
                           )                    
      ((lambda (s / d r)
               (while (setq d (tblnext s (null d)))
                 (setq r (cons (cdr (assoc 2 d)) r))
                 )
               )
               "block"
               )
        )
             )
           )
       (updatelist "allblock" (setq all_block_list nil))
       (updatelist "fixblock" fix_block_list)
       (set_tile "fixblock" "0")
    )

  (defun move_to ( Fromlst Tolst indexlist / i tmp )
;;;    (setq Fromlst all_block_list)
;;;    (setq Tolst fix_block_list)
;;;    (setq indexlist '(0 1 2 3 4))
    (setq i -1 tmp nil)
    (repeat (length Fromlst)
      (if (vl-position (setq i (1+ i)) indexlist)
        (if (not (vl-position (nth i Fromlst) Tolst))
          (setq Tolst (cons (nth i Fromlst) Tolst))
          )
        (setq tmp (cons (nth i Fromlst) tmp))
        )
      )
    (list (ACAD_STRLSORT(reverse tmp))(ACAD_STRLSORT(reverse Tolst)))
    )
  (defun synclist (key1 key2  / indexlist tmp lst1 lst2 k1 k2)
    (setq k1 (get_tile key1)
          k2 (get_tile key2)
          indexlist (read (strcat "(" k1 ")"))
          )
    (if (= key1 "allblock")
      (setq lst1 all_block_list lst2 fix_block_list)
      (setq lst2 all_block_list lst1 fix_block_list)
      )
    (setq tmp (move_to lst1 lst2 indexlist))
    (setq lst1 (car tmp) lst2 (cadr tmp))
    (UpdateList key1 lst1)
    (UpdateList key2 lst2)
;;;    (if (<= (apply 'max (read (strcat "(" k1 ")")))(1-(length lst1)))
;;;        (set_tile key1 k1)
;;;         (set_tile key1 "0")
;;;        )
;;;    (if (<= (apply 'max (read (strcat "(" k2 ")")))(1-(length lst2)))
;;;        (set_tile key2 k2)
;;;        (set_tile key2 "0")
;;;        )
    (set_tile key1 k1)(set_tile key2 k2)
    (if (= key1 "allblock")
      (setq all_block_list (car tmp) fix_block_list (cadr tmp))
      (setq all_block_list (cadr tmp) fix_block_list (car tmp))
      )
    )
  (defun initopt ( )
;;;Line Type
     (if (bg:bitset opt 1)(set_tile "o11" "1")(set_tile "o11" "0"))
     (if (bg:bitset opt 2)(set_tile "o12" "1")(set_tile "o12" "0"))
     (if (bg:bitset opt 4)(set_tile "o13" "1")(set_tile "o13" "0"))
;;;COlor
     (if (bg:bitset opt 8) (set_tile "o21" "1")(set_tile "o21" "0"))
     (if (bg:bitset opt 16)(set_tile "o22" "1")(set_tile "o22" "0"))
     (if (bg:bitset opt 32)(set_tile "o23" "1")(set_tile "o23" "0"))
;;;Lineweight
     (if (bg:bitset opt 64) (set_tile "o31" "1")(set_tile "o31" "0"))
     (if (bg:bitset opt 128)(set_tile "o32" "1")(set_tile "o32" "0"))
     (if (bg:bitset opt 256)(set_tile "o33" "1")(set_tile "o33" "0"))
;;;Layer 0
     (if (bg:bitset opt 512)(set_tile "layer" "1")(set_tile "layer" "0"))
;;;Pline width
     (if (bg:bitset opt 1024)(set_tile "plw" "1")(set_tile "plw" "0"))
;;; Unnamed
    (if (bg:bitset opt 2048)(set_tile "ux" "1")(set_tile "ux" "0"))
;;; Lock layer
    (if (bg:bitset opt 4096)(set_tile "lock" "1")(set_tile "lock" "0"))
    (if (bg:bitset opt 8192)(set_tile "su" "1")(set_tile "su" "0"))
    (if (bg:bitset opt 16384)(set_tile "ae" "1")(set_tile "ae" "0"))
    
    )
    (defun _GetSavePath ( / tmp )
    (cond      
      ( (setq tmp (getvar 'ROAMABLEROOTPREFIX))

        (strcat (vl-string-right-trim "\\" (vl-string-translate "/" "\\" tmp)) "\\Support")
      )
      ( (setq tmp (findfile "ACAD.pat"))

        (vl-string-right-trim "\\" (vl-string-translate "/" "\\" (vl-filename-directory tmp)))
      )
    )
  )
  (defun getopt ( / tmp)
           (+
            (if (= (get_tile "o11") "1") 1 0)
            (if (= (get_tile "o12") "1") 2 0)
            (if (= (get_tile "o13") "1") 4 0)
            
            (if (= (get_tile "o21") "1") 8 0)
            (if (= (get_tile "o22") "1") 16 0)
            (if (= (get_tile "o23") "1") 32 0)
            
            (if (= (get_tile "o31") "1") 64 0)
            (if (= (get_tile "o32") "1") 128 0)
            (if (= (get_tile "o33") "1") 256 0)

            (if (= (get_tile "layer") "1") 512 0)
            (if (= (get_tile "plw") "1") 1024 0)
            (if (= (get_tile "ux") "1") 2048 0)
            (if (= (get_tile "lock") "1") 4096 0)
            
            (if (= (get_tile "su") "1") 8192 0)
            (if (= (get_tile "ae") "1") 16384 0)
            )
    )
   (defun UpdateList ( key lst )

    ;; This function updates the list_box associated with the specified key
    ;; using the contents of the supplied lst

    (start_list key)
    (mapcar 'add_list lst)
    (end_list)
  )
  (defun _WriteDcl (fname / ofile IzRus)
    (setq IzRus (= (getvar "DWGCODEPAGE") "ANSI_1251"))
    (cond
      (
       (findfile fname)
      )
      ((setq ofile (open fname "w"))

       (foreach line
                (list
                  (strcat "//---------------------=={ BgBlFix.dcl v."(bg:ver)" }==--------------------//")
                  "//                                                            //"
                  "//  Dialog Definition file for use in conjunction with        //"
                  "//  Bgtools.lsp                                               //"
                  "//------------------------------------------------------------//"
                  "//  Author: Vladimir Azarko                                   //"
                  "//------------------------------------------------------------//"
                  "FIXBLOCK : dialog {label=\"FIXBLOCK\";key = \"tile\";"
(strcat ":row{:boxed_column {label=" (if IzRus "\"Все блоки\"" "\"All block list\"") ";")
":list_box {width= 31;height= 26;multiple_select = true ;key = \"allblock\";}}"
":column{fixed_width=true;alignment=centered;:spacer{width=0;height=5;}"
":button {key = \"b1\";label = \"---->\";fixed_width=true;alignment=centered;}"
":button {key = \"b2\";label = \"<----\";fixed_width=true;alignment=centered;}"
":button {key = \"b3\";label = \">>>\";fixed_width=true;alignment=centered;}"
":button {key = \"b4\";label = \"<<<\";fixed_width=true;alignment=centered;}"
(strcat ":row{:button {key = \"select\";label =" (if IzRus "\"Выбор >\"" "\"Select >\"") ";fixed_width = true;alignment = centered;height=3;}}")
":spacer{width=0;height=5;}}"
(strcat ":boxed_column{label=" (if IzRus "\"Блоки для нормализации\"" "\"Fix block list\"") ";:list_box{width=31;height=26;multiple_select=true;key=\"fixblock\";}}}")
(strcat "spacer_1;:boxed_row{label=" (if IzRus "\"Опции\"" "\"Options\"") ";" ":boxed_row{label=\"\";fixed_width=true;alignment=left;:column{")
(strcat ":text{label=" (if IzRus "\"    Типлинии\"" "\"    LineType\"") ";}")
(strcat ":text{label="(if IzRus "\"    Цвет\"" "\"    Color\"") ";}")
(strcat ":text{label="(if IzRus "\"    Вес\"" "\"    LineWeight\"") ";}")
"}:column{:row{label=\"\";fixed_width=true;alignment=left;"
(strcat ":radio_button {key=\"o11\";label=" (if IzRus "\"поблоку\"" "\"byblock\"") ";value=\"1\";}")
(strcat ":radio_button {key=\"o12\";label=" (if IzRus "\"послою\"" "\"bylayer\"") ";value=\"0\";}")
(strcat ":radio_button {key=\"o13\";label="(if IzRus "\"не менять\"" "\"skip\"")";value=\"0\";}")
"}:row{fixed_width=true;alignment=left;"
(strcat ":radio_button {key=\"o21\";label=" (if IzRus "\"поблоку\"" "\"byblock\"") ";value=\"1\";}")
(strcat ":radio_button {key=\"o22\";label=" (if IzRus "\"послою\"" "\"bylayer\"") ";value=\"0\";}")
(strcat ":radio_button {key=\"o23\";label="(if IzRus "\"не менять\"" "\"skip\"")";value=\"0\";}}")
":row{label=\"\";fixed_width=true;alignment=left;"
(strcat ":radio_button {key=\"o31\";label=" (if IzRus "\"поблоку\"" "\"byblock\"") ";value=\"1\";}")
(strcat ":radio_button {key=\"o32\";label=" (if IzRus "\"послою\"" "\"bylayer\"") ";value=\"0\";}")
(strcat ":radio_button {key=\"o33\";label="(if IzRus "\"не менять\"" "\"skip\"")";value=\"0\";}")
"}}}spacer_1;:boxed_column{spacer_1;"
(strcat ":toggle {key = \"layer\"; label=" (if IzRus "\"Слой на \\\"0\\\"\"" "\"Layer to \\\"0\\\"\"") ";}")
"spacer_1;"
(strcat ":toggle{key=\"plw\";label=" (if IzRus "\"Ширина полилиний=0\"" "\"Pline width=0\"") ";}")
"spacer_1;"                  
(strcat ":toggle{key=\"ux\";label=" (if IzRus "\"Включая неименованные (*U)\"" "\"Use unnamed (*U)\"") ";}")
"spacer_1;}}"
":row{"
(strcat ":toggle{key=\"lock\";label=\"" (if IzRus "Игнорировать заблокированные слои" "Ignore locked layers") "\";}")
(strcat ":toggle{key=\"su\";label=\"" (if IzRus "Установить одинаковые масштабы" "Scale uniformly") "\";}")
(strcat ":toggle{key=\"ae\";label=\"" (if IzRus "Разрешить расчленение" "Allow exploding") "\";}")                  
"}"                  
 "spacer_1;ok_cancel;}"
  )
         (write-line line ofile)
       ) ;_ end of foreach
       (setq ofile (close ofile))
       (findfile fname)
      )
    ) ;_ end of cond
  ) ;_defun
          
  (defun selectb ( / ss i lst bn)
    (if (setq ss (ssget "_:L" '((0 . "INSERT"))))
      (setq i (sslength ss))
      (setq i 0)
      )
    (setq lst fix_block_list)
    (repeat i  ;_ end setq
         (setq bn (bg:block-get-name (ssname ss (setq i(1- i)))))
         (if (not (VL-POSITION bn lst))(setq lst (cons bn lst)))
         ) ;
    (setq fix_block_list lst)
    (mapcar '(lambda (x)(setq all_block_list (vl-remove x all_block_list))) fix_block_list)
    )
  (setq Express
    (and (vl-position "acetutil.arx" (arx))
      (not
        (vl-catch-all-error-p
          (vl-catch-all-apply
            (function (lambda nil (acet-sys-shift-down)))
          )
        )
      )
    )
  )
  (if (not (vl-file-directory-p (setq what (_GetSavePath))))
    (progn
    (cond
        (Express
          (acet-ui-message "Save Path not Valid" "Warning" 16)
        )
        (t
          (princ "\n** Save Path not Valid **")
        )
      )
    (exit)
    )
  )
  ;;=== End func
(setq dcl (strcat what "\\BGBlFix_V" (bg:ver) ".dcl"))
(setq what nil opt 3657)
;;;(setq what nil opt 30281)
  
(_WriteDCL dcl)  
(if
      (not 
        (and 
          (setq dcl (findfile dcl))    ;; Check for DCL file
          (< 0 (setq dch (load_dialog dcl)))  ;; Attempt to load it if found
        )
      )
  (cond
        (Express
          (acet-ui-message "DCL File not found" "Warning" 16)
        )
        (t
          (princ "\n** DCL File not found **")
        )
      )
      (setq what 3)
    )
(sync1)
(while (and what (> what 2))
  (cond
(
      (not (new_dialog "FIXBLOCK" dch "" (cond ( *screenpoint* ) ( '(-1 -1) ))))

      ;; If our global variable *screenpoint* has a value it will be
      ;; used to position the dialog, else the default (-1 -1) will be
      ;; used to center the dialog on screen.

      ;; Should the dialog definition not exist, we unload the dialog
      ;; file from memory and inform the user:
                                             
      (setq dch (unload_dialog dch))
      (princ "\n** Dialog could not be Loaded **")
    )
    (t
       (initopt)
       (updatelist "allblock" all_block_list)
       (updatelist "fixblock" fix_block_list)
       (set_tile "fixblock" "0")
       (set_tile "allblock" "0")
      (set_tile "tile" (strcat "BGBLFIX v." (bg:ver)))
      (action_tile "accept" "(setq opt (getopt) *screenpoint* (done_dialog 1))")
      (action_tile "b1" "(synclist \"allblock\" \"fixblock\")")
      (action_tile "b2" "(synclist \"fixblock\" \"allblock\")")
      (action_tile "b3" "(sync2)")
      (action_tile "b4" "(sync1)")
      (action_tile "select" "(setq opt (getopt) *screenpoint* (done_dialog 4))")
      (action_tile "ux" "(_move_ux)")
      (setq what (start_dialog))
      (cond ((= what 4)
             (selectb)
             (updatelist "allblock" all_block_list)
             (updatelist "fixblock" fix_block_list)
             )
            ((= what 1)
;;;             (princ "\nFixblock= ")
;;;             (princ fix_block_list)
;;;             (princ "\nOptions = ")
;;;             (princ opt)(princ "\n")
             );;;Accept
             (t (setq what nil fix_block_list nil opt nil))
            )
    )
  );_cond
  ) ;_while what
  (if fix_block_list (cons opt fix_block_list))
  )


;;;		;;;Line Type
;;;		     (if (bg:bitset opt 1)(set_tile "o11" "1")(set_tile "o11" "0"))
;;;		     (if (bg:bitset opt 2)(set_tile "o12" "1")(set_tile "o12" "0"))
;;;		     (if (bg:bitset opt 4)(set_tile "o13" "1")(set_tile "o13" "0"))
;;;		;;;COlor
;;;		     (if (bg:bitset opt 8) (set_tile "o21" "1")(set_tile "o21" "0"))
;;;		     (if (bg:bitset opt 16)(set_tile "o22" "1")(set_tile "o22" "0"))
;;;		     (if (bg:bitset opt 32)(set_tile "o23" "1")(set_tile "o23" "0"))
;;;		;;;Lineweight
;;;		     (if (bg:bitset opt 64) (set_tile "o31" "1")(set_tile "o31" "0"))
;;;		     (if (bg:bitset opt 128)(set_tile "o32" "1")(set_tile "o32" "0"))
;;;		     (if (bg:bitset opt 256)(set_tile "o33" "1")(set_tile "o33" "0"))
;;;		;;;Layer 0
;;;		     (if (bg:bitset opt 512)(set_tile "layer" "1")(set_tile "layer" "0"))
;;;		;;;Pline width
;;;		     (if (bg:bitset opt 1024)(set_tile "plw" "1")(set_tile "plw" "0"))
;;;		;;; Unnamed
;;;		    (if (bg:bitset opt 2048)(set_tile "ux" "1")(set_tile "ux" "0"))

(defun bg:blfix ( lst / cnt adoc opt ss cnta isRus locklay bobj)
  ;;; lst - list, return (setq lst (bg:blfixdialog)) lst=(option "DKL1" BLK2" BLK3" ... )
  ;;; locklay - t -unlock lay nil - not
  (setq cnt 0 opt (car lst) lst (cdr lst) isRus (= (getvar "DWGCODEPAGE") "ANSI_1251"))
  (setq locklay (bg:bitset opt 4096))
  (and locklay (bg:layer-status-save))
  (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark adoc)
  (bg:progress-init "FIX Block Definition " (length lst))
  (foreach bname lst
    (bg:progress (setq cnt (1+ cnt)))
    (vlax-for sub_item (setq bobj (vla-item (vla-get-blocks adoc) bname))
      (cond
        ((bg:bitset opt 1)  ;;_Line type byblock
         (if (vlax-write-enabled-p sub_item)
           (vla-put-linetype sub_item "ByBlock")
           )
         )
        ((bg:bitset opt 2)  ;;_Line type bylayer
         (if (vlax-write-enabled-p sub_item)
           (vla-put-linetype sub_item "byLayer")
           )
         )
        (t nil)
        )
      (cond
        ((bg:bitset opt 8)  ;;_Color type byblock
         (if (vlax-write-enabled-p sub_item)
           (vla-put-color sub_item acByBlock)
           )
         )
        ((bg:bitset opt 16)  ;;_Color type bylayer
         (if (vlax-write-enabled-p sub_item)
           (vla-put-color sub_item acByLayer)
           )
         )
        (t nil)
        )
      (cond
        ((bg:bitset opt 64)  ;;_LineWeigth type byblock
         (if (vlax-write-enabled-p sub_item)
           (vla-put-LineWeight sub_item aclnwtbyblock)
           )
         )
        ((bg:bitset opt 128)  ;;_LineWeigth type bylayer
         (if (vlax-write-enabled-p sub_item)
           (vla-put-LineWeight sub_item acLnWtByLayer)
           )
         )
        (t nil)
        )
        (if (bg:bitset opt 512)  ;;_Layer to 0
          (if (vlax-write-enabled-p sub_item)
            (vla-put-Layer sub_item "0")
            )
         )
        (if (and  (bg:bitset opt 1024)(vlax-property-available-p sub_item "ConstantWidth"));_ Pline width
         (vl-catch-all-apply
           '(lambda ()
              (vla-put-ConstantWidth sub_item 0.)
              ) ;_ end of lambda
           ) ;_ end of vl-catch-all-apply
         )
      (if (and  (bg:bitset opt 8192)(eq (vla-get-ObjectName sub_item) "AcDbBlockReference"));_ Scale Uniformly
         (vl-catch-all-apply
           '(lambda ()
              (vla-put-XScaleFactor sub_item 1)
              (vla-put-YScaleFactor sub_item 1)
              (vla-put-ZScaleFactor sub_item 1)
              ) ;_ end of lambda
           ) ;_ end of vl-catch-all-apply
         )
      )
    (if (bg:bitset opt 16384)  ;;_Allow expolding
      (vla-put-Explodable bobj :vlax-true)
      ;;;(vla-put-Explodable bobj :vlax-false)
      )
    )
  (bg:progress-clear)
(if (setq ss (ssget "_X" (list (cons 0 "INSERT")(cons 66 1)))) ;;;Fix attribute
    (progn
      (setq cnta 0)
      (bg:progress-init "FIX Attribute " (sslength ss))
      (foreach blk (mapcar (function vlax-ename->vla-object) (vl-remove-if (function listp)
	  (mapcar (function cadr) (ssnamex ss))))
        (bg:progress (setq cnta (1+ cnta)))
        (if (vl-position (bg:block-get-name blk) lst)
          (foreach at (append (vlax-invoke blk 'GETATTRIBUTES)
                                 (vlax-invoke blk 'GETCONSTANTATTRIBUTES)
                                 )
      (cond
        ((bg:bitset opt 1)  ;;_Line type byblock
         (if (vlax-write-enabled-p at)
           (vla-put-linetype at "ByBlock")
           )
         )
        ((bg:bitset opt 2)  ;;_Line type bylayer
         (if (vlax-write-enabled-p at)
           (vla-put-linetype at "byLayer")
           )
         )
        (t nil)
        )
      (cond
        ((bg:bitset opt 8)  ;;_Color type byblock
         (if (vlax-write-enabled-p at)
           (vla-put-color at acByBlock)
           )
         )
        ((bg:bitset opt 16)  ;;_Color type bylayer
         (if (vlax-write-enabled-p at)
           (vla-put-color at acByLayer)
           )
         )
        (t nil)
        )
      (cond
        ((bg:bitset opt 64)  ;;_LineWeigth type byblock
         (if (vlax-write-enabled-p at)
           (vla-put-LineWeight at aclnwtbyblock)
           )
         )
        ((bg:bitset opt 128)  ;;_LineWeigth type bylayer
         (if (vlax-write-enabled-p at)
           (vla-put-LineWeight at acLnWtByLayer)
           )
         )
        (t nil)
        )
        (if (bg:bitset opt 512)  ;;_Layer to 0
          (if (vlax-write-enabled-p at)
            (vla-put-Layer at "0")
            )
         )            
            )
          )
        )
        (bg:progress-clear)
      )
    )
  (vla-endundomark adoc)
  (and locklay (bg:layer-status-restore))
  (vla-regen adoc acallviewports)
  (princ (if isRus
           (strcat "\nИзменено " (itoa cnt) " блоков")
           (strcat "\nFixed " (itoa cnt) " blocks")
           )
         )
  (princ)
  )

(defun c:BGResetXRef
       ( / *error* _Settings _GetDocumentObject acapp acdoc acdocs acver dbxdoc dcl def han inc lst props sel tile tiles xrf xreflst )

;;-------------------=={ Reset XRef Layers }==--------------------------;;
;; http://www.theswamp.org/index.php?topic=40132.0                      ;;
;;                                                                      ;;
;;;                                                                     ;;
;;  This program enables the user to reset all or specific layer        ;;
;;  properties of xref dependent layers to match the properties         ;;
;;  present in the xref source drawing file.                            ;;
;;                                                                      ;;
;;  Upon starting the program, the user is prompted to select an xref   ;;
;;  whose layers are to be reset. Following a valid selection, the      ;;
;;  properties of all layers dependent on the selected xref are reset   ;;
;;  to match the values found in the source drawing for the selected    ;;
;;  xref.                                                               ;;
;;                                                                      ;;
;;  From the selection prompt, the user may also choose 'Multiple',     ;;
;;  'All', or 'Settings'.                                               ;;
;;                                                                      ;;
;;  If 'Multiple' is selected, the user may select several xrefs using  ;;
;;  the standard selection interface (e.g. via window selection).       ;;
;;                                                                      ;;
;;  If 'All' is selected, the layer properties of every xref found in   ;;
;;  the active drawing is reset to match the original values found in   ;;
;;  the respective source drawings.                                     ;;
;;                                                                      ;;
;;  Finally, if the 'Settings' option is selected, a dialog interface   ;;
;;  is displayed allowing the user to choose which layer properties     ;;
;;  are to be reset.                                                    ;;
;;                                                                      ;;
;;  The user may select multiple properties to be reset from: Colour,   ;;
;;  Linetype, Lineweight, Plot, Plot Style, Frozen in Viewports, On,    ;;
;;  Locked, Frozen, & Description.                                      ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-11-19                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2011-11-27                                      ;;
;;                                                                      ;;
;;  - Added code to search for XRef Source File in working directory &  ;;
;;    support directories if not found at XRef Path.                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2014-04-13                                      ;;
;;                                                                      ;;
;;  - Program entirely rewritten.                                       ;;
;;  - Modified program to account for layers whose colour property      ;;
;;    uses a True Colour or Colour Book colour.                         ;;
;;----------------------------------------------------------------------;;
       


    (defun *error* ( msg )
        (if (< 0 han) (setq han (unload_dialog han)))
        (if (and dcl  (setq dcl (findfile dcl))) (vl-file-delete dcl))
        (if (and dbxdoc (not (vlax-object-released-p dbxdoc))) (vlax-release-object dbxdoc))
        (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (defun _Settings ( code / bit dcl han tmp )
        (cond
            (
                (not
                    (and
                        (setq dcl (vl-filename-mktemp nil nil ".dcl"))
                        (setq tmp (open dcl "w"))
                        (progn
                            (foreach line
                              (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                                '(
                                    "rxl : dialog { label = \"Настройки\"; spacer;"
                                    "    : boxed_column { label = \"Свойства для восстановления\"; width = 65.0; fixed_width = true; alignment = centered; spacer;"
                                    "        : row { alignment = centered; spacer; "
                                    "            : column {"
                                    "                : toggle { key = \"colour\";     label = \"Цвет\"; }"
                                    "                : toggle { key = \"linetype\";   label = \"Тип линии\"; }"
                                    "                : toggle { key = \"lineweight\"; label = \"Вес линии\"; }"
                                    "            }"
                                    "            : column {"
                                    "                : toggle { key = \"plot\";      label = \"Печать\"; }"
                                    "                : toggle { key = \"plotstyle\"; label = \"Стиль печати\"; }"
                                    "                : toggle { key = \"frozenvp\";  label = \"Заморожен на новых ВЭ\"; }"
                                    "            }"
                                    "            : column {"
                                    "                : toggle { key = \"on\";     label = \"Вкл\"; }"
                                    "                : toggle { key = \"locked\"; label = \"Блокировать\"; }"
                                    "                : toggle { key = \"frozen\"; label = \"Заморозить\"; }"
                                    "            }"
                                    "            : column {"
                                    "                : toggle { key = \"description\"; label = \"Описание\"; }"
                                    "                spacer;"
                                    "                : toggle { key = \"selectall\";   label = \"Выбрать все\"; }"
                                    "            }"
                                    "        }"
                                    "        spacer;"
                                    "    }"
                                    "    spacer; ok_cancel;"
                                    "}"
                                )
                               '(
                                    "rxl : dialog { label = \"Settings\"; spacer;"
                                    "    : boxed_column { label = \"Properties to Reset\"; width = 65.0; fixed_width = true; alignment = centered; spacer;"
                                    "        : row { alignment = centered; spacer; "
                                    "            : column {"
                                    "                : toggle { key = \"colour\";     label = \"Colour\"; }"
                                    "                : toggle { key = \"linetype\";   label = \"Linetype\"; }"
                                    "                : toggle { key = \"lineweight\"; label = \"Lineweight\"; }"
                                    "            }"
                                    "            : column {"
                                    "                : toggle { key = \"plot\";      label = \"Plot\"; }"
                                    "                : toggle { key = \"plotstyle\"; label = \"Plot Style\"; }"
                                    "                : toggle { key = \"frozenvp\";  label = \"Frozen in VP\"; }"
                                    "            }"
                                    "            : column {"
                                    "                : toggle { key = \"on\";     label = \"On\"; }"
                                    "                : toggle { key = \"locked\"; label = \"Locked\"; }"
                                    "                : toggle { key = \"frozen\"; label = \"Frozen\"; }"
                                    "            }"
                                    "            : column {"
                                    "                : toggle { key = \"description\"; label = \"Description\"; }"
                                    "                spacer;"
                                    "                : toggle { key = \"selectall\";   label = \"Select All\"; }"
                                    "            }"
                                    "        }"
                                    "        spacer;"
                                    "    }"
                                    "    spacer; ok_cancel;"
                                    "}"
                                )
                                )
                                (write-line line tmp)
                            )
                            (setq tmp (close tmp))
                            (while (null (findfile dcl)))
                            (< 0 (setq han (load_dialog dcl)))
                        )
                        (new_dialog "rxl" han)
                    )
                )
                (princ "\nError Loading Dialog.")
            )
            (   t
                (setq bit 1
                      tmp code
                )
                (if (= 1023 tmp)
                    (set_tile "selectall" "1")
                )
                (foreach tile
                    (setq tiles
                       '(
                            "colour"
                            "linetype"
                            "lineweight"
                            "plot"
                            "plotstyle"
                            "frozenvp"
                            "on"
                            "locked"
                            "frozen"
                            "description"
                        )
                    )
                    (if (= bit (logand tmp bit))
                        (set_tile tile "1")
                        (set_tile tile "0")
                    )
                    (action_tile tile
                        (strcat
                            "(setq tmp (boole 6 tmp " (itoa bit) "))"
                            "(set_tile \"selectall\" (if (= 1023 tmp) \"1\" \"0\")))"
                        )
                    )
                    (setq bit (lsh bit 1))
                )
                (action_tile "selectall"
                    (strcat
                        "(foreach tile tiles (set_tile tile $value))"
                        "(if (eq \"1\" $value)"
                        "    (setq tmp 1023)"
                        "    (setq tmp 0)"
                        ")"
                    )
                )                        
                (if (= 1 (start_dialog)) (setq code tmp))
            )
        )
        (if (< 0 han) (setq han (unload_dialog han)))
        (if (and dcl  (setq dcl (findfile dcl))) (vl-file-delete dcl))
        code
    )

    (defun _GetDocumentObject ( dbxdoc acdocs xref / path xpath )
        (setq xpath (cdr (assoc 1 (entget (tblobjname "BLOCK" xref)))))
        (cond
            (   (null
                    (or
                        (setq path (findfile xpath))
                        (setq path (findfile (strcat (vl-filename-base xpath) ".dwg")))
                    )
                )
                (princ (strcat "\nSource Drawing for " xref " not Found."))
                nil
            )
            (   (cdr (assoc path acdocs))
            )
            (   (null
                    (vl-catch-all-error-p
                        (vl-catch-all-apply 'vla-open (list dbxdoc path))
                    )
                )
                dbxdoc
            )
            (   (princ (strcat "\nUnable to Open " xref " Source Drawing."))
                nil
            )
        )
    )

    (if (null (setq props (getenv "LMac\\RXLProps")))
        (setq props (+ 1 2 4 8 512))
        (setq props (atoi props))
    )

    (while (setq def (tblnext "BLOCK" (null def)))
        (if (= 4 (logand 4 (cdr (assoc 70 def))))
            (setq lst (cons "," (cons (cdr (assoc 2 def)) lst)))
        )
    )

    (cond
        (   (null lst)
            (princ "\nNo XRefs found in Drawing.")
        )
        (   t
            (setq acapp  (vlax-get-acad-object)
                  acdoc  (vla-get-activedocument acapp)
                  dbxdoc (vla-GetInterfaceObject acapp
                             (if (< (setq acver (atoi (getvar "ACADVER"))) 16)
                                 "ObjectDBX.AxDbDocument"
                                 (strcat "ObjectDBX.AxDbDocument." (itoa acver))
                             )
                         )
                  acdocs (vlax-for doc (vla-get-documents acapp)
                             (setq acdocs (cons (cons (vla-get-fullname doc) doc) acdocs))
                         )
            )
            (while
                (progn
                    (setvar 'ERRNO 0)
                    (initget "Несколько Все Имя Настройки Multiple All Name Settings _Multiple All Name Settings Multiple All Name Settings")
                    (setq sel (entsel
                                (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                                "\nВыберите XRef для сброса настроек или [Несколько/Все/выбор по Имени/Настройки] <Выход>: "  
                                "\nSelect XRef to Reset or [Multiple/All/select by Name/Settings] <Exit>: "
                                  )
                                ))
                    (cond
                        (   (= 7 (getvar 'ERRNO))
                            (princ "\nMissed, try again.")
                        )
                        (   (null sel)
                            nil
                        )
                        (   (eq "Multiple" sel)
                            (setvar 'NOMUTT 1)
                            (princ(if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                                    "\nВыберите XRefs для сброса настроек <Выход>: "
                                    "\nSelect XRefs to Reset <Exit>: "
                                    )
                                  )
                            (setq sel
                                (vl-catch-all-apply 'ssget
                                    (list
                                        (list '(0 . "INSERT") (cons 2 (apply 'strcat (cdr lst))))
                                    )
                                )
                            )
                            (setvar 'NOMUTT 0)
                            (if (and sel (not (vl-catch-all-error-p sel)))
                                (repeat (setq inc (sslength sel))
                                    (LM:ResetXRefLayers
                                        (setq xrf (cdr (assoc 2 (entget (ssname sel (setq inc (1- inc)))))))
                                        (_GetDocumentObject dbxdoc acdocs xrf)
                                        acdoc
                                        props
                                    )
                                )
                            )
                            nil
                        )
                        (   (eq "All" sel)
                            (while (setq def (tblnext "BLOCK" (null def)))
                                (if (= 4 (logand 4 (cdr (assoc 70 def))))
                                    (LM:ResetXRefLayers
                                        (setq xrf (cdr (assoc 2 def)))
                                        (_GetDocumentObject dbxdoc acdocs xrf)
                                        acdoc
                                        props
                                    )
                                )
                            )
                            nil
                        )
                        (   (eq "Name" sel)
                         (while (setq def (tblnext "BLOCK" (null def)))
                           (if (= 4 (logand 4 (cdr (assoc 70 def))))
                             (setq xreflst (cons (cdr (assoc 2 def)) xreflst))
                                )
                           )
                          (foreach xrf (_dwgru-get-user-dcl
                                         (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                                         "Выберите ссылки "
                                         "Select XRefs"
                                           )
                                         (acad_strlsort xreflst) t)
                            (LM:ResetXRefLayers
                                        xrf
                                        (_GetDocumentObject dbxdoc acdocs xrf)
                                        acdoc
                                        props
                                    )
                            )
                            nil
                        )
                        
                        (   (eq "Settings" sel)
                            (setq props (_Settings props))
                        )
                        (   (vl-consp sel)
                            (if (eq "INSERT" (cdr (assoc 0 (setq sel (entget (car sel))))))
                                (if (= 4 (logand 4 (cdr (assoc 70 (tblsearch "BLOCK" (cdr (assoc 2 sel)))))))
                                    (LM:ResetXRefLayers
                                        (setq xrf (cdr (assoc 2 sel)))
                                        (_GetDocumentObject dbxdoc acdocs xrf)
                                        acdoc
                                        props
                                    )
                                    (princ "\nSelected Block is not an XRef.")
                                )
                                (princ "\nInvalid Object Selected.")
                            )
                        )
                    )
                )
            )
            (vla-regen acdoc acallviewports)
            (vlax-release-object dbxdoc)
        )
    )
    (setenv "LMac\\RXLProps" (itoa props))
    (princ)
)
(defun LM:ResetXRefLayers ( xref xrdoc acdoc props / _GetLayerProperties ass bit data name pos value xdef )
;;;  Lee Mac 2011 www.lee-mac.com
    (defun _GetLayerProperties ( doc props / bit lst data )
        (vlax-for layer (vla-get-layers doc)
            (setq bit 1
                  lst nil
            )
            (foreach prop
               '(
                    color
                    linetype
                    lineweight
                    plottable
                    plotstylename
                    viewportdefault
                    layeron
                    lock
                    freeze
                    description
                )
                (if
                    (and
                        (vlax-property-available-p layer prop)
                        (= bit (logand bit props))
                    )
                    (setq lst (cons (cons bit (vlax-get-property layer prop)) lst))
                )
                (setq bit (lsh bit 1))
            )
            (setq data
                (cons
                    (cons
                        (strcase (vla-get-name layer))
                        (reverse lst)
                    )
                    data
                )
            )
        )
        data
    )
    
    (cond
        (   (null xrdoc)
            nil
        )
        (   (vl-catch-all-error-p
                (setq xdef
                    (vl-catch-all-apply 'vla-item (list (vla-get-blocks acdoc) xref))
                )
            )
            (princ "\nXRef not present in Drawing.")
            nil
        )
        (   (setq data (_GetLayerProperties xrdoc props))
            (vla-reload xdef)

            (vla-startundomark acdoc)
            (vlax-for layer (vla-get-layers acdoc)
                (setq bit 1)
                (if
                    (and
                        (setq pos (vl-string-position 124 (setq name (strcase (vla-get-name layer)))))
                        (eq (strcase xref) (substr name 1 pos))
                        (setq ass (cdr (assoc (substr name (+ 2 pos)) data)))
                    )
                    (foreach prop
                       '(
                            color
                            linetype
                            lineweight
                            plottable
                            plotstylename
                            viewportdefault
                            layeron
                            lock
                            freeze
                            description
                        )
                        (if
                            (and
                                (vlax-property-available-p layer prop t)
                                (= bit (logand bit props))
                                (setq value (cdr (assoc bit ass)))
                            )
                            (if (and (= 2 bit) (not (eq "CONTINUOUS" (strcase value))))
                                (vl-catch-all-apply 'vlax-put-property (list layer prop (strcat xref "|" value)))
                                (vl-catch-all-apply 'vlax-put-property (list layer prop value))
                            )
                        )
                        (setq bit (lsh bit 1))
                    )
                )
            )
            (vla-endundomark acdoc)
            t
        )
    )
)



(defun c:BGAll2RGB ( / accm c e i s _UpdateTrueColor _UpdateTrueColorNested )
;;; All to RGB  -  Lee Mac  -  www.lee-mac.com
;;; Converts the ACI colours of all entities to the RGB TrueColor equivalent
;;;http://www.cadtutor.net/forum/showthread.php?65645-Convert-from-index-colours-to-true-colours&s=b86c725f0e1c2793ae84f0c7babc7f59
  (defun _UpdateTrueColor ( e c)
    (if (eq c 7) ;_Black/Wнite
      (vla-SetRGB accm 0 0 0)
      (vla-put-colorindex accm c)
      )
    (entmod
      (append e
              (list
                (cons 420
                      (LM:RGB->True
                        (vla-get-red   accm)
                        (vla-get-green accm)
                        (vla-get-blue  accm)
                        )
                      )
                )
              )
      )
    )
  (defun _UpdateTrueColorNested ( e / c)
;;;  (setq e (car(entsel)))
(if (and
      (eq "INSERT" (cdr (assoc 0 e)))
      (setq e (tblobjname "BLOCK" (cdr (assoc 2 e))))
      )
  (while (setq e (entnext e))
    (if (and (setq c (cdr (assoc 62 (entget e))))
             (not (zerop c))
            )
      (_UpdateTrueColor (entget e) c)
      )
    (if (eq "INSERT" (cdr (assoc 0 (entget e))))
      (_UpdateTrueColorNested (entget e))
      )
    )
  )
  )
    (if
        (and
            (setq s (ssget "_:L"))
            (setq accm
                (vla-getinterfaceobject (vlax-get-acad-object)
                    (strcat "AutoCAD.AcCmColor." (substr (getvar 'ACADVER) 1 2))
                )
            )
        )
        (progn
            (repeat (setq i (sslength s))
                (setq e (entget (ssname s (setq i (1- i)))))
                (if (and (setq c (cdr (assoc 62 e)))
                         (not (assoc 420 e))
                         )
                    (_UpdateTrueColor e c)
                )
              (_UpdateTrueColorNested e)
            )
            (vlax-release-object accm)
        )
    )
  (vl-cmdf "_regenall")
    (princ)
)
;;;================================================================================
;;;Written By Michael Puckett. 
;;;Список элементов символьных таблиц АвтоКАДа 
;;; - s- имя таблицы
;;;Пример - список всех слоев - (setq all_layers (tablelist "LAYER"))
;;;(setq all_layers (tablelist "LAYER"))
;;;
;;;AutoLisp should return something like this :
;;;Start Coding Here 
    (defun tablelist (s / d r)
      (while (setq d (tblnext s (null d)))
        (setq r (cons (cdr (assoc 2 d)) r))
      );_ while
    );_ defun
;;;End Coding Here	

(defun LM:RGB->True ( r g b )
;; RGB -> True - Lee Mac 2011
;; Args: r,g,b - Red,Green,Blue values

    (+
        (lsh (fix r) 16)
        (lsh (fix g)  8)
        (fix b)
    )
)
(defun bg:truecolor->RGB ( truecolor )
  ;;;USE (BG:truecolor->RGB 8227990 )
(list
   (logand (lsh truecolor -16) 255) ;; R
   (logand (lsh truecolor -8) 255)  ;; G
   (logand truecolor 255)           ;; B
)
)
;;------------------=={ Copy/Rename Block }==-----------------;;
;;                                                            ;;
;;  Copies or Renames an single selected block reference with ;;
;;  a name specified by the user. The program utilises an     ;;
;;  ObjectDBX Document interface to copy the block definition ;;
;;  of the selected reference, perform the rename operation,  ;;
;;  then copy the renamed definion back to the working        ;;
;;  drawing.                                                  ;;
;;                                                            ;;
;;  Program works with Dynamic Blocks & XRefs.                ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.3    -    25-08-2011                            ;;
;;------------------------------------------------------------;;
;; url: http://lee-mac.com/copyblock.html                     ;;
;;------------------------------------------------------------;;
(defun c:BGCB nil (RenameBlock   t))

(defun c:BGRB nil (RenameBlock nil))

;;------------------------------------------------------------;;

(defun RenameBlock ( copy / *error* _Name _ReleaseObject acapp acdoc b1 b2 d1 dbdoc df n1 n2 )

;;------------------------------------------------------------;;

  (defun *error* ( msg )
    (_ReleaseObject dbdoc)
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _Name ( obj )
    (if (vlax-property-available-p obj 'EffectiveName)
      (vla-get-EffectiveName obj)
      (vla-get-Name obj)
    )
  )

  (defun _ReleaseObject ( obj )
    (and obj (eq 'VLA-OBJECT (type obj)) (not (vlax-object-released-p obj))
      (not
        (vl-catch-all-error-p
          (vl-catch-all-apply 'vlax-release-object (list obj))
        )
      )
    )
  )
  
;;------------------------------------------------------------;;

  (setq acapp (vlax-get-acad-object)
        acdoc (vla-get-activedocument acapp)
        acblk (vla-get-blocks acdoc)
  )
  
  (if
    (and
      (zerop(getvar "BLOCKEDITOR"))
      (setq b1
        (car
          (LM:Selectif
            (strcat
              (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                "\nВыберите ссылку блока для "
                "\nSelect Block Reference to "
                )
              (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
                (if copy "Копии" "Переименования")
                (if copy "Copy" "Rename")
                )
                ": ")
            (lambda ( x ) (eq "INSERT" (cdr (assoc 0 (entget (car x))))))
            entsel nil
          )
        )
      )
      (LM:CopyBlockDef acdoc (setq dbdoc (LM:ObjectDBXDocument acapp)) (setq n1 (_Name (setq b1 (vlax-ename->vla-object b1))))
        (progn
          (while
            (progn
              (setq n2
                (getstring t
                  (strcat (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\nВведите новое имя блока <" "\nSpecify New Block Name <")
                    (setq df
                      (
                        (lambda ( i / b )
                          (while
                            (tblsearch "BLOCK"
                              (setq b (strcat n1 "_" (itoa (setq i (1+ i)))))
                            )
                          )
                          b
                        )
                        0
                      )
                    )
                    "> : "
                  )
                )
              )
              (cond
                ( (eq "" n2) (setq n2 df)
                  nil
                )
                ( (or (not (snvalid n2)) (tblsearch "BLOCK" n2))
                  (princ (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "\nНеверное или Существующее имя блока." "\nBlock Name Invalid or Already Exists."))
                )
              )
            )
          )
          n2
        )
      )
    )
    (progn
      (if (and (vlax-property-available-p b1 'isDynamicBlock) (eq :vlax-true (vla-get-isDynamicBlock b1)))
        (progn
          (setq p1 (mapcar 'vla-get-value (vlax-invoke b1 'GetDynamicBlockProperties)))
          (vla-put-name (if copy (setq b1 (vla-copy b1)) b1) n2)
          (mapcar
            (function
              (lambda ( a b )
                (or (eq "ORIGIN" (strcase (vla-get-PropertyName a))) (vla-put-value a b))
              )
            )
            (vlax-invoke b1 'GetDynamicBlockProperties) p1
          )
        )
        (vla-put-name (if copy (setq b1 (vla-copy b1)) b1) n2)
      )
      (if (eq :vlax-true (vla-get-isxref (setq d1 (vla-item acblk n2))))
        (vla-reload d1)
      )
      (if copy (sssetfirst nil (ssadd (vlax-vla-object->ename b1))))
    )
    (if (not(zerop(getvar "BLOCKEDITOR")))
      (bg:msg-Popup
        (if (= (getvar "DWGCODEPAGE") "ANSI_1251") "Внимание" "Attention")
        (if (= (getvar "DWGCODEPAGE") "ANSI_1251")
          "Команда не работает в редакторе блоков!"
          "Command is not working in the Block Editor!"
          )
        16
        )
      )
  )
  (_ReleaseObject dbdoc)
  (princ)
)

;;---------------=={ Copy Block Definition }==----------------;;
;;                                                            ;;
;;  Copies the specified block defintion with new name as     ;;
;;  specified                                                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  acdoc - Document Object containing Block to copy          ;;
;;  dbdoc - ObjectDBX Document                                ;;
;;  name1 - Name of block definition to copy                  ;;
;;  name2 - Name to be used for copied definition             ;;
;;------------------------------------------------------------;;
;;  Returns: Copied VLA Block Definition Object, else nil     ;;
;;------------------------------------------------------------;;
;;http://www.lee-mac.com/copyblockdefinition.html
(defun LM:CopyBlockDef ( acdoc dbdoc name1 name2 / acblk dbblk b1 b2 )
  (setq acblk (vla-get-blocks acdoc)
        dbblk (vla-get-blocks dbdoc)
  )               
  (if
    (and
      (setq b1 (LM:GetItem acblk name1))
      (not     (LM:GetItem acblk name2))
      (not(wcmatch name2 "`**")) ;;;Add VVA 2012-03-03
    )
    (progn  
      (vla-CopyObjects acdoc (LM:SafearrayVariant vlax-vbObject (list b1)) dbblk)
      (vla-put-Name (setq b2 (LM:GetItem dbblk name1)) name2)
      (vla-CopyObjects dbdoc (LM:SafearrayVariant vlax-vbObject (list b2)) acblk)
    )
  )
  (LM:GetItem acblk name2)
)

;;--------------=={ VLA-Collection: Get Item }==--------------;;
;;                                                            ;;
;;  Retrieves the item with index 'item' if present in the    ;;
;;  specified collection, else nil                            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  collection - the VLA Collection Object                    ;;
;;  item       - the index of the item to be retrieved        ;;
;;------------------------------------------------------------;;
;;  Returns:  the VLA Object at the specified index, else nil ;;
;;------------------------------------------------------------;;

(defun LM:GetItem ( collection item )
  (if
    (not
      (vl-catch-all-error-p
        (setq item (vl-catch-all-apply 'vla-item (list collection item)))
      )
    )
    item
  )
)

;;-----------------=={ ObjectDBX Document }==-----------------;;
;;                                                            ;;
;;  Retrieves a version specific ObjectDBX Document object    ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  acapp - AutoCAD VLA Application Object                    ;;
;;------------------------------------------------------------;;
;;  Returns:  VLA ObjectDBX Document object, else nil         ;;
;;------------------------------------------------------------;;

(defun LM:ObjectDBXDocument ( acapp / acVer )
  (vla-GetInterfaceObject acapp
    (if (< (setq acVer (atoi (getvar "ACADVER"))) 16)
      "ObjectDBX.AxDbDocument" (strcat "ObjectDBX.AxDbDocument." (itoa acVer))
    )
  )
)

;;------------------=={ Safearray Variant }==-----------------;;
;;                                                            ;;
;;  Creates a populated Safearray Variant of a specified      ;;
;;  data type                                                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  datatype - variant type enum (eg vlax-vbDouble)           ;;
;;  data     - list of static type data                       ;;
;;------------------------------------------------------------;;
;;  Returns:  VLA Variant Object of type specified            ;;
;;------------------------------------------------------------;;

(defun LM:SafearrayVariant ( datatype data )
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray datatype (cons 0 (1- (length data)))) data
    )
  )
)

;;---------------------=={ Select if }==----------------------;;
;;                                                            ;;
;;  Provides continuous selection prompts until either a      ;;
;;  predicate function is validated or a keyword is supplied. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg  - prompt string                                      ;;
;;  pred - optional predicate function [selection list arg]   ;;
;;  func - selection function to invoke                       ;;
;;  keyw - optional initget argument list                     ;;
;;------------------------------------------------------------;;
;;  Returns:  Entity selection list, keyword, or nil          ;;
;;------------------------------------------------------------;;
;; USE
;;;(LM:Selectif
;;;  "\nВыберите ссылку "
;;;  (lambda ( x ) (eq "INSERT" (cdr (assoc 0 (entget (car x))))))
;;;  entsel
;;;  nil
;;;)

(defun LM:SelectIf ( msg pred func keyw / sel ) (setq pred (eval pred))  
  (while
    (progn (setvar 'ERRNO 0) (if keyw (apply 'initget keyw)) (setq sel (func msg))
      (cond
        ( (= 7 (getvar 'ERRNO))
          (princ "\nMissed, Try again.")
        )
        ( (eq 'STR (type sel))
          nil
        )
        ( (vl-consp sel)
          (if (and pred (not (pred sel)))
            (princ "\nInvalid Object Selected.")
          )
        )
      )
    )
  )
  sel
)


(defun bg:bitset (A B) (= (boole 1 A B) B))
(defun bg:ver () "2.31")
(if (= (getvar "DWGCODEPAGE") "ANSI_1251")
(progn
  (princ (strcat "\nBackground tools v." (bg:ver) " Наберите в командной строке:"))
  (princ "\nBGLAYDEL - Удаление выключенных и замороженных слоев с примитивами")
  (princ "\nBG1BLEXP - Взрывает блоки, состоящие из одного примитива (возможно другого блока)")
  (princ "\nBGBLEXP  - Взрывает блок и все блоки, входящие в него, превращая видимые атрибуты в текст")
  (princ "\nBGBLEXP1 - Взрывает блоки 1-го уровня, превращая видимые атрибуты в текст")
  (princ "\nBGBLDYNEXP1 - Взрывает динамические блоки 1-го уровня, превращая видимые атрибуты в текст")
  (princ "\nBGBLEXP1NOATT - Взрывает блоки 1-го уровня, у которых нет видимых атрибутов")
  (princ "\nBGBLXCLIP - Показывает подрезанные блоки")
  (princ "\nBGBLXCLIPEXP - Взорвать подрезанный блок")
  (princ "\nBGBLDYN2A - Преобразовывает динамические блоки в анонимные")
  (princ "\nBGBLDYN2S - Преобразовывает динамические блоки в статические")
  (princ "\nBGBLALLDYN2A - Преобразовывает ВСЕ динамические блоки в анонимные")
  (princ "\nBGBLALLDYN2S - Преобразовывает ВСЕ динамические блоки в статические")
  (princ "\nBGCB - копирует одно из вхождений блока в блок с новым именем. www.lee-mac.com")
  (princ "\nBGRB - переименовывает одно из вхождений блока в блок с новым именем. www.lee-mac.com")
  (princ "\nM2U - Преобразовать MINSERT (МН-БЛОК) в Unnamed блок")
  (princ "\nM2B - Преобразовать MINSERT (МН-БЛОК) в блок")
  (princ "\nU2B - Преобразовать UNNAMED в блок")
  (princ "\nU2BM - Преобразовать UNNAMED в блок множественный выбор")
  (princ "\nUX - Взорвать UNNAMED блок")
  (princ "\n=======================================")
  (princ "\nBGRGB2ACI - Изменяет цвет с RGB на ACI")
  (princ "\nBGAll2RGB - Изменяет ACI цвет в его RGB эквивалент")
  (princ "\nBGCOLOR - Задает выбранный цвет всем объектам")
  (princ "\nBGCOLORXREF - Меняет цвет выбранных внешних сылок (только в текушем сеансе)")
  (princ "\nBGResetXRef -Сброс настроек слоев выбранных внешних ссылок на установленные в исходном файле")
  (princ "\nBGBLCC - Изменяет цвет выбранных блоков")
  (princ "\nBGENCC - Изменяет цвет выбранного элемента блока")
  (princ "\nBGCATT - Изменяет цвет атрибутов выбранных блоков")
  (princ "\nBGCBL2 - Изменяет цвет в блоках 2 и более уровня (вложенных блоках)")
  (princ "\nBGBLFIX - Нормализация блоков")
  (princ "\nBGCFT   - Конвертирует все поля в обычный текст")
  (princ "\nBGCFTSEL - конвертирование полей в обычный текст в выбранных примитивах")
  (princ "\nBGHATCHDEL - Удаление штриховки")
  )
(progn
  (princ (strcat "\nBackground tools v." (bg:ver) " Type in command line:"))
  (princ "\nBGLAYDEL - delete frozen and off layers with objects")
  (princ "\nBG1BLEXP - Explode blocks consisting of one primitive thing (it is possible other block)")
  (princ "\nBGBLEXP  - Explode the block and all blocks entering into it, transforming visible attributes in the text")
  (princ "\nBGBLEXP1 - Explode blocks of 1st level, transforming visible attributes in the text")
  (princ "\nBGBLDYNEXP1 - Explode dynamic blocks of 1st level, transforming visible attributes in the text")
  (princ "\nBGBLEXP1NOATT -Explode blocks of 1st level which do not have visible attributes ")
  (princ "\nBGBLXCLIP - show xclip blocks")
  (princ "\nBGBLDYN2A - Convert Dynamic Blocks to Anonymous blocks")
  (princ "\nBGBLALLDYN2A - Conver ALL Dynamic Blocks to Anonymous")
  (princ "\nBGBLALLDYN2S - Conver ALL Dynamic Blocks to Static")
  (princ "\nBGBLDYN2S - Convert Dynamic Blocks to Static blocks")
  (princ "\nBGBLXCLIP - show xclip blocks")
  (princ "\nBGBLXCLIPEXP - explode clipped block")
  (princ "\nBGCB - copy a single block reference in the working drawing. www.lee-mac.com")
  (princ "\nBGRB - rename a single block reference in the working drawing. www.lee-mac.com")
  (princ "\nM2U - Convert MINSERT to Unnamed block")
  (princ "\nM2B - Convert MINSERT to block")
  (princ "\nU2B - Convert UNNAMED to block")
  (princ "\nU2BM - Convert UNNAMED to block multiple selection")
  (princ "\nUX - Explode UNNAMED block")
  (princ "\n=======================================")
  (princ "\nBGRGB2ACI - Changes color from RGB to the corresponding Index Color (ACI)")
  (princ "\nBGAll2RGB - Converts the ACI colours of all entities to the RGB TrueColor equivalent")
  (princ "\nBGCOLOR - Set a selected color to all objects (WITHOUT XREF)")
  (princ "\nBGCOLORXREF - Changes color selected XREF ( ONLY ON A CURRENT SESSION )")
  (princ "\nBGResetXRef -Resets properties of all layers dependent on the selected XREF(s) to those set in the source drawing file(s).")
  (princ "\nBGBLCC - Changes color of the chosen blocks")
  (princ "\nBGENCC - Changes color of the chosen element of the block")
  (princ "\nBGCATT - Changes color of attributes of the chosen blocks")
  (princ "\nBGCBL2 - Changes color in blocks 2 and more levels (the nested blocks)")
  (princ "\nBGBLFIX - fix (normalize) blocks")
  (princ "\nBGCFT - Convert field to text")
  (princ "\nBGCFTSEL - Convert field to text in selected objects")
  (princ "\nBGHATCHDEL - Remove hatch")
    )
  )
(princ)
(c:BGBLXCLIPEXP)
