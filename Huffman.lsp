
(defun freq (stri &optional (res nil))
  (cond ((= 0 (strlen stri)) res)
        (t (let* ((first (strLeft stri 1))
                  (l1 (strlen stri))
                  (stro (strRep stri first ""))
                  (l2 (strlen stro)))
          (freq stro (cons (list first (- l1 l2)) res))))))

(defun bit2num (llst &optional (a 0))
    (if (null llst) a
       (bit2num (cdr llst) (+ (* a 2) (car llst)))))

(defun make-tree (lst)
  (cond ((null (cdr lst)) (car lst))
        (t  (let* ((plst (qsort-a lst (lambda (x) (cadr x))))
                   (a1   (car  plst))
                   (a2   (cadr plst))
                   (n    (list "_" (+ (cadr a1) (cadr a2)) a1 a2)))
         (make-tree (cons n (cddr plst)))))))

(defun psi-1 (x &optional (lst nil))
   (cond ((null (cddr x)) (list (cons (car x) (press lst))))
         (t (let ((a (psi-1 (caddr x) (append lst (list 0))))
                  (b (psi-1 (cadddr x) (append lst (list 1)))))
               (append a b)))))

(defun sortik (lst str &optional(res ""))
   (cond ((= 0 (strlen str)) res)
      (t (sortik lst (strMid str 2) (strCat res (cdr (assoc (strleft str 1) lst)))))))

(defun str2list (str)
  (iter(for a in-string str)(collecting a)))

(defun cut (x n)
  (let ((lx (length x)))
       (if (>= lx n) (cons (subseq x 0 n) (cut (subseq x n) n))
                     (if (null x) nil (list x)))))

(defun unite-num(x &optional (result ""))
   (cond ((null x) result)           
         (t (unite-num (cdr x) (strCat result (strChr (bit2num (car x))))))))

(defun zapakovka (str c)
   (unite-num (cut (mapcar 'str2fix (str2list (sortik c str))) 8)))

(defun zapakovka-1 (str с)
   (sortik с str))

(defun swap (pairs)
  (mapcar (lambda (p) (cons (cdr p) (car p))) pairs))

(defun antisortic (lst stri &optional (curr "") (result ""))
      (cond ((= 0 (strlen stri)) 
                  (let ((tmp (assoc curr lst)))
                          (if (null tmp) result  (strCat result (cdr tmp)))))
            (t (let ((tmp (assoc curr lst))) 
                    (if (null tmp) ;; Код не найден (пока)
                        (antisortic lst (strMid stri 2) (strCat curr (strLeft stri 1)) result)
                        (antisortic lst (strMid stri 2) (strLeft stri 1) (strCat result (cdr tmp))))))))

(defun raspakovka (lst str)
   (antisortic (swap lst) str))

(defun press (list)
  (let ((res ""))
    (iter (for a in list) (setq res (strCat res (fix2str a)))) res))

(defun halfman (str)
   (let ((c (psi-1 (make-tree (qsort-a (freq str) (lambda (x) (cadr x)))))))
         (progn (printsline (strCat "Изначальная строка:" str))
                (printsline (strCat "Изначальное кол-во байтов:" (strlen str)))
                (printsline (strCat "Строка после запаковки:"(zapakovka str c)))
                (printsline (strCat "Длина строки после запаковки:" (strlen (zapakovka str c))))
                (printsline (strcat "Строка после распаковки:" (raspakovka c (zapakovka-1 str c)))) (qsort-a (freq str) (lambda (x) (cadr x))))))

(defun drw_tree (x &optional ( y (/ 600 (length x)))  (z (\ 600 (+ 1 (* 3 (length x))))))
 (cond ((null (cdr x)) (grwCircle 'h (cadr (car x)) (caddr (car x)) z _black) (grwFont 'h "Times New Roman" 14 T Nil)  (grwPrint 'h  (- (cadr(car x)) 2) (- (nth 2 (car x))2 )  (caar x) _black))
       (t (drw_tree (append (list (list (+ (caar x) (caadr x))  (\ (+ (cadr (car x)) (cadr (cadr x))) 2) (+ (- (nth 2 (car x)) y)))) (cddr x)) y z)
          (grwCircle 'h (cadr(car x)) (nth 2 (car x)) z _black) (grwFont 'h "Times New Roman" 14 T Nil)  (grwPrint 'h  (- (cadr(car x)) 2) (- (nth 2 (car x))2 )  (caar x) _black) 
 )))

(defun drw_tree0 (x &optional ( y (/ 600 (length x)))  (z (\ 600 (+ 1 (* 3 (length x))))))
 (cond ((null (cdr x)) (grwCircle 'h (cadr (car x)) (caddr (car x)) z _black)) 
       (t  (drw_tree0 (append (list (list (+ (caar x) (caadr x))  (\ (+ (cadr (car x)) (cadr (cadr x))) 2) (+ (- (nth 2 (car x)) y)))) (cddr x)) y z)
           (grwCircle 'h (cadr(car x)) (nth 2 (car x)) z _black)  )))

(defun finish (x &optional ( y (/ 600 (length x))))
 (cond ((null (cdr x)) nil) 
       (t (finish (append (list (list (+ (caar x) (caadr x))  (\ (+ (cadr (car x)) (cadr (cadr x))) 2) (+ (- (nth 2 (car x)) y)))) (cddr x)) y)
          (grwLine 'h (cadr(car x)) (nth 2 (car x)) (\ (+ (cadr (car x)) (cadr (cadr x))) 2) (+ (- (nth 2 (car x)) y)) (grwRGB 255 192 203)) 
          (grwLine 'h (cadr(cadr x)) (nth 2 (cadr x)) (\ (+ (cadr (car x)) (cadr (cadr x))) 2) (+ (- (nth 2 (car x)) y)) (grwRGB 255 192 203)))))

(defun kruzhki1 (lst &optional (x (*  2 (\  600 (+ 1 (* (length lst) 3))))) (y (- 600 (\ 600 (* 2 (length lst))))) (z (\ 600 (+ 1 (* 3 ( length lst)))))(result nil))
   (cond ((null lst) result)
         (t (progn (grwfill 'h x y (grwRGB 255 192 203) _black) 
                   (append result (list (list (cadr(car lst)) x y )) (kruzhki1 (cdr lst)  (+ x (* 3  z))  y z))))))

(defun kruzhki0  (lst &optional (x (*  2 (\  600 (+ 1 (* (length lst) 3))))) (y (- 600 (\ 600 (* 2 (length lst))))) (z (\ 600 (+ 1 (* 3 ( length lst)))))(result nil))
   (cond ((null lst) result)
  (t (progn (grwcircle 'h x y z _black) 
            (append result (list (list (cadr(car lst)) x y )) (kruzhki0 (cdr lst)  (+ x (* 3  z))  y z))))))

(defun kruzhki (lst &optional (x (*  2 (\  600 (+ 1 (* (length lst) 3))))) (y (- 600 (\ 600 (* 2 (length lst))))) (z (\ 600 (+ 1 (* 3 ( length lst)))))(result nil))
   (cond ((null lst) result)
  (t (progn (grwCircle 'h x y z _black) (grwFont 'h "Times New Roman" 14 T Nil)  (grwPrint 'h (- x 2 ) (- y 2) (cadr (car lst)) _black) 
            (append result (list (list (cadr(car lst)) x y )) (kruzhki (cdr lst)  (+ x (* 3  z))  y z))))))

(defun windows(&optional (x 600)(y 600) (color (grwRGB 175 238 238)) (name 'h))
    (grwCreate name x y "Дерево Хаффмана" color)
      (grwShow name))
	  
 (defun drw_tree1 (x &optional ( y (/ 600 (length x)))  (z (\ 600 (+ 1 (* 3 (length x))))))
 (cond ((null (cdr x)) (grwfill 'h (cadr (car x)) (caddr (car x)) (grwrgb 255 192 203) _black))
       (t   (drw_tree1 (append (list (list (+ (caar x) (caadr x))  (\ (+ (cadr (car x)) (cadr (cadr x))) 2) (+ (- (nth 2 (car x)) y)))) (cddr x)) y z)          (grwfill 'h (cadr(car x)) (nth 2 (car x)) (grwrgb 255 192 203) _black) 
 )))

(defun tree (x)
   (windows)
   (grwsetparm 'h 5 nil nil nil)
   (let ((c (kruzhki0 (reverse x))))
         (kruzhki1 (reverse x))
         (drw_tree0 (reverse c))
         (drw_tree1 (reverse c))
         (finish   (reverse c))
         (kruzhki (reverse x))
         (drw_tree (reverse c))))

(defun start (str)
 (tree (halfman str)))

//
//  Программа отображения диалога _Dlg_
//

(defun main nil
      (try (dlgDestroy '_Dlg_) except Nil)

      (dlgCreate '_Dlg_ 377 159 "Сжатие Хаффмана" &H8000000F)

      (dlgAddControl '_Dlg_ '_BUT_1 _BUTTON 12 15 78 48 '("Tahoma" 8,25 1 0 0) "Сжать")
      (dlgPutPicture '_BUT_1 4)

      (dlgAddControl '_Dlg_ '_TXT_1 _TEXT 95 21 250 31 '("Tahoma" 14 1 0 0) "" 0 &H80000008 &H80000005)

      (dlgAddControl '_Dlg_ '_BUT_2 _BUTTON 14 66 333 45 '("Tahoma" 8,25 1 0 0) "Закрыть")
      (dlgPutPicture '_BUT_2 36)

      //
      // Пролог загрузки диалога _Dlg_
      //

      (Prog () 

      )

      //
      // Обработчик события CLICK для кнопки _BUT_1
      //

      (defun _BUT_1_Click  Nil 

        (start (dlgGetText '_TXT_1))

      )

      //
      //   Назначение процедуры-события _BUT_1_Click  контролу _BUT_1
      //

      (dlgSetEvent '_BUT_1 '_BUT_1_Click )

      //
      // Обработчик события CLICK для кнопки _BUT_2
      //

      (defun _BUT_2_Click  Nil 

         (dlgHide '_DLG_)
         (dlgDestroy '_DLG_) 

      )

      //
      //   Назначение процедуры-события _BUT_2_Click  контролу _BUT_2
      //

      (dlgSetEvent '_BUT_2 '_BUT_2_Click )

      //
      //   Отображение диалога _Dlg_
      //

      (dlgShow '_Dlg_)
)

