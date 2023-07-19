(defun antisublim1 (n m) 
    (PROGN (FILOPEN 'NACH N 0)
           (FILOPEN 'KON M 1) 
       (LET* ((NACH (FILGET 'NACH))
              (tmp (strwords nach :delim "=")))
            (printsline nach) 
          (FILPUTLINE 'KON (antisortic (bin2rat (car (strwords nach :delim "="))) (input (cadr (strwords nach :delim "="))) (bin2rat2 (caddr (strwords nach :delim "=")))))
          (FILCLOSE 'NACH)
          (FILCLOSE 'KON))))
		  
(defun bin2rat2 (N)
   (SETQ N (REVERSE (STR2LIST N)))
   (LET ((K 1)
         (ANS 0)) 
	 (PROGN (ITER (FOR I IN N) 
	         (PROGN (WHEN (= I 1) (SETQ ANS (+ ANS K))) (SETQ K (* K 2)))) ANS)))

(defun STR2LIST (N) (REVERSE (CDR (REVERSE (CDR (EXPLODE N))))))
		  
(defun rat2bin2 (N) 
   (LET ((ANS 0)
         (ANSR "1") 
		 (J 1)
		 (I 0)) 
	(LOOP 
          (IF (> J N) (RETURN 0) 
		  (PROGN (SETQ J (* J 2))
   		         (SETQ I (+ I 1))))) 
				 (SETQ J (\ J 2))
				 (SETQ N (- N J))
				 (SETQ J (\ J 2))
				 (SETQ I (- I 1))
				 (PROGN (ITER (FOR K FROM 1 TO I)
  				      (PROGN (IF (>= N J) (PROGN (SETQ N (- N J)) 
					                             (SETQ ANSR (STRCAT ANSR "1")))
										  (SETQ ANSR (STRCAT ANSR "0")))
							 (PRINTS N)
							 (PRINTSLINE J)
							 (SETQ J (/ J 2)))) ANSR)))
							 
(defun RAT2BIN (N M) (LET 
		((ANS 0) 
		(ANSR "0.") 
		(J 1/2)) 
		(PROGN 
			(LOOP (IF (AND (<= ANS M) (>= ANS N)) (RETURN 0) 
				(IF (< (+ ANS J) M) 
					(PROGN 
						(SETQ ANSR (STRCAT ANSR "1")) (SETQ ANS (+ ANS J)) (SETQ J (/ J 2))) 
						(PROGN 
						  (SETQ ANSR (STRCAT ANSR "0")) 
						  (SETQ J (/ J 2)))))) ANSR))) 
(defun CHETCHIK (N) 
   (LET ((G (LENGTH N))) 
        (PROGN 
            (LET ((ANS NIL)
	              (UNIQ NIL) 
	              (LENPR 0) 
	              (LENNEW 0)) 
	            (ITER (FOR I IN N) (UNLESS (MEMBER I UNIQ) (SETQ UNIQ (APPEND UNIQ (LIST I))))) 
		              (ITER (FOR J IN UNIQ) 
		                 (SETQ LENPR (LENGTH N)) 
		                 (SETQ N (REMOVE J N)) 
		                 (SETQ LENNEW (LENGTH N)) 
		                 (SETQ ANS (APPEND ANS (LIST (LIST J (/ (- LENPR LENNEW) G))))))
		    ANS))))
		
(defun filget (N) 
	(LET ((ANS "")) 
	(LOOP (IF (FILEOF N) 
		(RETURN ANS) 
		(SETQ ANS (STRCAT ANS (FILGETLINE N)))))))
		
(defun bin2rat (N) 
	(SETQ N (CDDR (STR2LIST N))) 
	(LET ((ANS 0) 
		(J 1/2)) 
		(PROGN 
		(ITER (FOR I IN N) 
			(PROGN (WHEN (= I 1) (SETQ ANS (+ ANS J))) (SETQ J (/ J 2)))) 
	ANS)))
		
(defun 2lines (DIN SIZE OKNO) 
	(PROGN 
		(GRWFILL OKNO 0 0 &HFFFFFF &H00FF00) 
		(GRWLINE OKNO (ROUND (RAT2FLO (* SIZE (CAR DIN))) 0) 0 (ROUND (RAT2FLO (* SIZE (CAR DIN))) 0) 100 &H008000)
		(GRWLINE OKNO (ROUND (RAT2FLO (* SIZE (CADR DIN))) 0) 100 (ROUND (RAT2FLO (* SIZE (CADR DIN))) 0) 200 &HFF0000))) 
		
(defun antisortic (KOD FREQ LEN) 
	(LET ((ANS NIL) (BNOW 0) (POG (SORTICK2 FREQ)) (DIN '(0 1)))
	(PROGN 
		(ITER (FOR J FROM 1 TO LEN)
			(ITER (FOR I IN POG) 
				(PROGN 
					(WHEN (>= (+ (* (CADDR I) (- (CADR DIN) (CAR DIN))) (CAR DIN)) KOD) 
						(PROGN (SETQ DIN (LIST (+ (* (CADR I) (- (CADR DIN) (CAR DIN))) (CAR DIN)) (+ (* (CADDR I) (- (CADR DIN) (CAR DIN))) (CAR DIN)))) 
							   (SETQ ANS (APPEND ANS (LIST (CAR I)))) (RETURN NIL)))))) 
	ANS)))	
	
(defun antisortik (N) 
	(ANTISORTIC (SORTIC N) (SORTIK N) (LENGTH N))) 
	
(defun SORTIK (N) (REVERSE (QSORT-A (CHETCHIK N) (LAMBDA (X) (CADR X))))) 

(defun SORTIC (N) 
	(LET ((M (SORTIK3 N)) (ANS (QUOTE (0 1))) (GNOW NIL) (ANSR 1)) 
		(PROGN 
			(ITER (FOR I IN N) 
				(SETQ GNOW (ASSOC I M)) 
				(SETQ ANSR (- (CADR ANS) (CAR ANS))) 
				(SETQ ANS (LIST (+ (CAR ANS) (* ANSR (- (CADDR GNOW) (CADR GNOW)))) (- (CADR ANS) (* ANSR (- 1 (CADDR GNOW))))))) 
	(/ (+ (car ANS) (cadr ANS)) 2) ))) 
	
(defun SORTIC2 (N) 
	(LET ((M (SORTIK3 N)) (ANS (QUOTE (0 1))) (GNOW NIL) (ANSR 1)) 
		(PROGN 
			(ITER (FOR I IN N) 
				(SETQ GNOW (ASSOC I M)) 
				(SETQ ANSR (- (CADR ANS) (CAR ANS))) 
				(SETQ ANS (LIST (+ (CAR ANS) (* ANSR (- (CADDR GNOW) (CADR GNOW)))) (- (CADR ANS) (* ANSR (- 1 (CADDR GNOW))))))) 
	ANS )))	
	
(defun SORTIK3 (N) 
	(PROGN 
		(LET ((M (REVERSE (QSORT-A (CHETCHIK N) (LAMBDA (X) (CADR X))))) (ANS NIL) (CK (SORTICK N))) 
			(ITER (FOR I IN M) 
					(FOR J IN CK) 
						(SETQ ANS (APPEND ANS (LIST (APPEND I (LIST J))))))
	ANS))) 
	
(defun SORTICK (N) 
	(LET ((M (SORTIK N)) (ANS NIL) (M2 NIL)) 
		(PROGN 
			(SETQ M (REVERSE M)) 
			(SETQ M2 (MAPCAR (QUOTE CADR) M)) 
			(REVERSE (MAPLIST (LAMBDA (X) (APPLY (QUOTE +) X)) 
	M2))))) 
	
(defun SORTICK2 (FRIKS) 
	(LET ((FRIKSOLD FRIKS)) 
		(SETQ FRIKS (REVERSE 
		(MAPCAR 'CADR FRIKS))) 
		(MAPCAR 'LIST 
		(MAPCAR 'CAR FRIKSOLD) (APPEND (LIST 0) (REVERSE (CDR (MAPLIST (LAMBDA (X) (APPLY '+ X)) FRIKS)))) 
		(REVERSE (MAPLIST (LAMBDA (X) (APPLY '+ X)) FRIKS))))) 
		
(defun sublim (N M) 
	(PROGN 
	(FILOPEN 'NACH N 0) (FILOPEN  'KON M 1) 
	(LET 
	((NACH (FILGET 'NACH))) 
		(FILPUTLINE 'KON 
		(STRCAT (FIX2STR (LENGTH (STR2LIST NACH))) " " 
		(OUTPUT (APPLY 'APPEND (SORTIK (STR2LIST NACH)))) " " 
		(RAT2BIN (SORTIC (STR2LIST NACH))))) 
		(FILCLOSE 'NACH) (FILCLOSE 'KON)))) 
		
(defun sublim1 (N M) 
	(PROGN 
	(FILOPEN 'NACH N 0) (FILOPEN 'KON M 1) 
	(LET 
		((NACH (FILGET 'NACH))) 
		(FILPUTLINE 'KON (STRCAT (APPLY 'RAT2BIN (SORTIC2SS (STR2LIST NACH))) "=" 
		(OUTPUT (SORTIK (STR2LIST NACH))) "=" (RAT2BIN2 (LENGTH (STR2LIST NACH))))) 
		(FILCLOSE 'NACH) (FILCLOSE 'KON))))

;; strWords писал не я, а Борис Леонидович.

(defun strWords (stri &key (delim " "))
  (let ((acc "")
        (res nil))
    (iter (for a in-string stri) 
      (cond ((eq a delim) (when (> (strLen acc) 0) (collecting acc into res)) (setq acc ""))
            (t (setq acc (strCat acc a)))))
    (if (> (strLen acc) 0) (append res (list acc)) res))) 	
	
(defun main nil
  (let* ((stri  (ask "Введите строку для сжатия:"))
         (len   (strlen stri))
		 (lstri (str2list stri))
         (press (sortic lstri))
		 (freq  (sortik lstri))
		 (res   (antisortic press freq len)))
	 (printsline (strCat "Исходная строка: " stri))
	 (printsline (strcat "Набор частот: " (OUTPUT freq)))
     (printsline (strCat "Результат сжатия: " (output press)))
     (printsline (strCat "Результат распаковки: " (output res)))
     'OK))

   	 
 
 
           
	