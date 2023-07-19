// 
// ���������� ������
//

(defun lzw-pack (stri &optional (res nil) (dict nil) (ss "") (flag Nil)) 

   ;; stri - ��������� ������
   ;; res  - ��������� ������ (����� 9-� ������ �����)
   ;; dict - �������
   ;; ss   - ������������� ����������   
   ;; flag - ������� ������ ���������

   (let ((font1 "<font face=Courier size=+1 color=Navy>")
         (font2 "<font face=Courier size=+1 color=Red>")   
		 (nl    "<br>")
		 (blank "&nbsp;")
		 (ff    "</font>"))
   
   (when (and flag (> (strlen stri) 0))
         (filPutLine 'fout (strCat font1 "������" blank ":" ff font2 (strLeft stri 1) blank ff nl))
		 (filPutLine 'fout (strCat font1 "�������:" ff font2 (output dict) blank ff nl ))
		 (filPutLine 'fout (strCat font1 "�������:" ff blank font2 ss ff nl nl)))
   
   (cond ((= 0 (strlen stri)) (append res (list (getCode ss dict))))  ;; ������������ ����� ��������
                                                                      ;; ����� ���������� ����
        (t (let* ((k (strLeft stri 1)) ;; ��������� ������ ������
                  (sk (strCat ss k)))  ;; sk=ss+k
                 (cond ((= 1 (strlen sk)) (lzw-pack (strMid stri 2) res dict sk flag)) ;; ��������. ������ ������ ���� � �������
                       ((assoc sk dict)   (lzw-pack (strMid stri 2) res dict sk flag)) ;; ������ ���� � �������
                       (t                                                         ;; ������ sk � ������� ���!
                             (lzw-pack (strMid stri 2)  ;; �������� ������ ��� 1-�� �������
							           (append res (list (getCode ss dict)))      ;; �������� � res ��������� ���
									   (putInDict sk dict) k flag))))))))               ;; �������� � ������� ��������� ���

(defun getCode (code dict)   ;; ������� ����������� 9 ������� ����
  (if (= 1 (strLen code)) (strAsc code) (cadr (assoc code dict)))) ;; ������� ����� ��������� � 9 ������ ����

(defun putInDict (code dict) ;; �������� ������� � ������
  (let ((k (+ 256 (length dict)))) ;; ����� �������� � ������
        (cons (list code k) dict))) ;; �������� ������� � ������

;; ����� -> ������

(defun dec2bin (n &optional (r nil)) ;;��������� ���������� ������� � ��������
   (if (= n 0) ;; ������� ��������������� ��������
       (if (null r) '(0) r) ;; ���� ��� �� ����� 0
           (dec2bin (\ n 2) (cons (% n 2) r))))

(defun bin2dec (blist &optional (a 0))
  (if (null blist) a (bin2dec (cdr blist) (+ (car blist) (* a 2)))))

;; ������������� ������ 9-������ � ������

(defun conv (lst)
  (let* ((tmp1 (apply 'append (mapcar 'dec2bin lst)))  ;; �������������� � ������ �����
         (lt   (length tmp1))                          ;; ����� ������
         (d    (% lt 8))                               ;; ��� �� ��� �� 8
         (tmp2 (append (iter (for _ from 1 to (- 8 d)) (collecting 0)) tmp1)) ;; ���������� ������ �� �����, ������� 8
         (l2   (/ (length tmp2) 8))                    ;; �-�� ������ ���������� 
		 (code 0)
         (res  ""))                                    ;; ���������
     (iter (for i from 0 to (- l2 1))                  ;; �������� 8-�� ����� � ����������� �� � ������
	    (setq code (bin2dec (subseq tmp2 (* i 8) (* (+ i 1) 8)))) 
        (setq res (strCat res (if (> code 31) (strChr code) ".") )))))

(defun start-pack ( &optional flag)
  (let* ((stri (ask "������� ������"))
         (l1 (strLen stri))
         (pstr (lzw-pack stri nil nil "" flag))
         (l2  (length pstr)))
       (printsline (strCat "�������� ������  : " stri))
       (printsline (strCat "����� � �����    : " (fix2str (* l1 8))))
       (printsline (strCat "��������� ������ : " (output pstr)))
       (printsline (strCat "� ���� ASCII     : " (conv pstr)))
       (printsline (strCat "����� � �����    : " (fix2str (* l2 9))))
       (printsline (strCat "�����. ������    : " (format (/ (* l1 8) (* l2 9.0)) "0.0"))) 'ok))

;; ����������  
;; ����: ������ 9-������ ����� 
;; ����� - ������

;; �������� � ������� ���

(defun add-in-dict (code dict)
   (cons (list (+ 256 (length dict)) code) dict))					 

(defun get-from-dict (code dict)
     (if (< code 256) (strChr code) (cadr (assoc code dict))))

(defun lzw-unpack (lst9 &optional flag)

   (let ((font1 "<font face=Courier size=+1 color=Navy>")
         (font2 "<font face=Courier size=+1 color=Red>")   
		 (nl    "<br>")
		 (blank "&nbsp;")
		 (ff    "</font>")
         (cCode (car lst9))
         (pCode (car lst9))
		 (dict nil)
		 (tmp1 nil)
         (tmp2 nil)		 
		 (res ""))
		 
	  (setq res (strCat res (strChr cCode)))
	  
      (iter (for cCode in (cdr lst9))
	        
 		    (setq tmp1 (get-from-dict cCode dict))
			(setq tmp2 (get-from-dict pCode dict))
			
			(when flag
				  
				  (filPutLine 'fout (strCat font1 "������� ���:" blank blank blank blank ff font2 (fix2str cCode) blank tmp1 ff nl))
				  (filPutLine 'fout (strCat font1 "���������� ���:" blank ff font2 (fix2str pCode) blank tmp2 ff nl))
				  (filPutLine 'fout (strCat font1 "�������:" blank blank blank blank blank blank blank blank ff font2 (output dict) ff nl nl)))
			
		    (cond (tmp1 
			       (setq res (strCat res tmp1)) ;; ����� cCode
				   (setq tmp1 (get-from-dict cCode dict))
				   (setq tmp2 (get-from-dict pCode dict))
				   (setq dict (add-in-dict (strCat tmp2 (strLeft tmp1 1)) dict)))
				  (t   (setq res (strCat res (strCat tmp2 (strLeft tmp2 1)))) 
				       (setq dict (add-in-dict (strCat tmp2 (strLeft tmp2 1)) dict))))
			(setq pCode cCode)) res ))
	                        
(defun start-unpack (&optional flag)
   (let* ((bcodes (input (ask "������� ������� ���:")))
          (res (lzw-unpack bcodes flag)))
	   (printsline (strCat "�������� ���: " (output bcodes)))
       (printsline (strCat "����� � �����: " (fix2str (* 9 (length bcodes)))))
       (printsline (strCat "��������� ����������: " res))
       (printsline (strCat "����� � �����: " (fix2str (* 8 (strLen res))))) 'OK ))
	
;; "(48 49 256 48 55 258 260 49)"
;; 010107010701					 
;; asdd dfg hh hg ff f hgdg iouhi ug herigiuehg iued higludhgiulguh giudehgiuhgiruhg iu1

(defun ask-ync (txt)
  (let ((answ nil))
    (loop (setq answ (strUCase (ask txt)))
          (cond ((member answ '("YES" "Y")) (return t))
                ((member answ '("NO" "N")) (return nil))
                ((member answ '("QUIT" "Q")) (return 0))))))

//
//  ��������� ����������� ������� _Dlg_
//

(defun main nil

      (try (dlgDestroy '_Dlg_) except Nil)
	  
	  (filCloseAll)

      (dlgCreate '_Dlg_ 630 433 "LZW-��������" &H8000000F)

      (dlgAddControl '_Dlg_ '_BUT_4 _BUTTON 5 64 200 86 '("Times New Roman" 20,25 1 0 0) "�����")
      (dlgPutPicture '_BUT_4 11)

      (dlgAddControl '_Dlg_ '_BUT_5 _BUTTON 210 64 200 86 '("Times New Roman" 20,25 1 0 0) "�������")
      (dlgPutPicture '_BUT_5 8)

      (dlgAddControl '_Dlg_ '_BUT_6 _BUTTON 415 64 200 86 '("Times New Roman" 20,25 1 0 0) "�������")
      (dlgPutPicture '_BUT_6 36)

      (dlgAddControl '_Dlg_ '_TXT_1 _TEXT 9 11 605 30 '("Times New Roman" 14 1 0 0) "������" 0 &H80000008 &H80000005)

      (dlgAddControl '_Dlg_ '_LBL_1 _LABEL 16 172 542 203 '("Courier New" 16 1 0 0) "������ � ������" 0 &H80000012 &H8000000F)

      //
      // ������ �������� ������� _Dlg_
      //

      ;; ������� ���������� ���������

      (prog nil 
        (setq *save* "")
		(setq *flag* (ask-ync "�������� �������� ������ (yes/no)?"))
		(when (eq *flag* 0) (return nil))
        (when *flag* 
		      (filOpen 'fout "lzw.html" _OUTPUT)
			  (filPutLine 'fout "<html>")
			  (filPutLine 'fout "<head> </head>")
			  (filPutLine 'fout "<body><b>")))

     (when (not (eq *flag* 0))  

      //
      // ���������� ������� CLICK ��� ������ _BUT_4
      //

      (defun _BUT_4_Click  Nil 

		(when *flag* (filPutLine 'fout (strCat "<br><font face=Courier size=+2 color=red> ��������: </font> <br><br>")))
		
        (let* ((stri (dlgGetText '_TXT_1))
               (l1 (strLen stri))
               (pstr (lzw-pack stri nil nil "" *flag*))
               (l2  (length pstr))
			   (out (strCat "�������� ������  : " stri (strChr 10)
        				    "����� � �����    : " (fix2str (* l1 8)) (strChr 10)
							"��������� ������ : " (output pstr)(strChr 10)
							"� ���� ASCII     : " (conv pstr) (strChr 10)
							"����� � �����    : " (fix2str (* l2 9)) (strChr 10)
							"�����. ������    : " (format (/ (* l1 8) (* l2 9.0)) "0.0")))) (setq *save* (output pstr))
		    (dlgPutText '_LBL_1 out)))

      //
      //   ���������� ���������-������� _BUT_4_Click  �������� _BUT_4
      //

      (dlgSetEvent '_BUT_4 '_BUT_4_Click )

      //
      // ���������� ������� CLICK ��� ������ _BUT_5
      //

      (defun _BUT_5_Click  Nil 

         (let* ((data *save*)
                (unpk nil))		 
			(cond ((= 0 (strLen data)) (say "������������� ������!"))
                  (t (when *flag* (filPutLine 'fout (strCat "<br><br> <font face=Courier size=+2 color=red> ����������: </font> <br><br>")))
				     (setq unpk (lzw-unpack (input data) *flag*))
			         (dlgPutText '_LBL_1 (strCat "��������� ����������: " unpk)))))) 	

      //
      //   ���������� ���������-������� _BUT_5_Click  �������� _BUT_5
      //

      (dlgSetEvent '_BUT_5 '_BUT_5_Click )

      //
      // ���������� ������� CLICK ��� ����� _LBL_1
      //

      (defun _BUT_6_Click  ()  
          (dlgHide '_DLG_)
		  (dlgDestroy '_DLG_)
		  (when *flag* 
		        (filPutLine 'fout "</b></body></html>")
		        (filClose 'fout)
				(sysShell (strCat (sysHome) "\lzw.html"))))

      //
      //   ���������� ���������-������� _LBL_1_Click  �������� _LBL_1
      //

      (dlgSetEvent '_BUT_6 '_BUT_6_Click )

      //
      //   ����������� ������� _Dlg_
      //

      (dlgShow '_Dlg_)))

;; ������ ������� ���������

;;(main)