;; ver.20220611
;; get schedule-image from city.hiroshima.jp

(ql:quickload :drakma)
(ql:quickload :dexador)
(ql:quickload :plump)
(ql:quickload :clss)
(ql:quickload :quri)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-csv)

;; variables
(defparameter *host* "www.sports-or.city.hiroshima.jp")
(defparameter *login* "http://www.sports-or.city.hiroshima.jp/info/sev")

(defparameter *pwd* (truename "."))
(defparameter *img-file* "schedule.jpg")
(defparameter *date-file* "up-to-date.txt")
(defparameter *csv-in* nil)
(defparameter *csv-out* nil)

;; I/O
(defun fname-str (fname)
  (merge-pathnames fname *pwd*))

(defun load-csv (&optional (fname *load*)) ; #P"file" ; csv -> list
  (setf *csv-in* (cl-csv:read-csv (fname-str fname) :separator #\tab)))

(defun save-csv (&optional (fname *save*)) ; #P"file" ; list -> csv
  (cl-csv:write-csv *csv-out* :stream (fname-str fname) :separator #\tab))

;; get-image
(defun login (&optional (url *login*))
  (let* ((html (drakma:http-request url))
         (parsed (plump:parse html)))

    (check-up-date parsed)))

(defun check-up-date (parsed)
  (let* ((t1 (clss:select ".contents" parsed)) ; parent-parsed 
         (t2 (clss:select "p" (aref t1 0)))    ; child-parsed
         (t3 (loop for n below (length t2) ; loop collect -> make list
                   collect (plump:text (aref t2 n)))) ; get text from <p>...</p>
         (t4 (format nil "~{~A~}" t3))
         (t5 (cl-ppcre:regex-replace-all "\\n" t4 ""))
         (checked-date (cl-ppcre:scan-to-strings "令和.年.月.日" t5))
         (stamped-date (caar (load-csv *date-file*))))

    (if (not (equal checked-date stamped-date)) ; equal ; for the strings
        (progn
          (format t "*** Need to update ***~%")
          (get-img parsed)
          (format t "new image saved !!~%")
          (up-date-file checked-date)
          (format t "new date saved !!~%"))
        (format t "*** Do not need to update ***~%"))))
  
(defun get-img (parsed)
  (let* ((vec (clss:select "#image-marker" parsed)) ;id検索はplss:selectと#利用
         (img-path (plump:attribute (aref vec 0) "src")) ;属性内容はplump:attributeと""を利用
         (img-url (quri:render-uri (quri:make-uri :scheme "http"
                                                  :host *host*
                                                  :path img-path)))
         (ymd (local-time:format-timestring nil (local-time:now) 
                                            :format '(:year :month :day)))
         (file-name (format nil "~A~A~A" *pwd* ymd *img-file*)))
  
    (dex:fetch img-url file-name)))

(defun up-date-file (checked-date)
  (setf *csv-out* (list checked-date))
  (save-csv *date-file*))

;; main
(defun main ()
  (login))
