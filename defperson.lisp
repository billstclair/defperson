;;; ILC/ELS badge design, for Avery 85x54mm 2x5 Business Card sheets.
;;;
;;; Basically,
;;;
;;;   (defperson:cards :people *people*)
;;;
;;; where *people* is a list of lists with various positional elements
;;;
;;; Either change *venue* or pass a :VENUE arg to CARDS.
;;;
;;; Output goes to /tmp/cards<ii>.pdf

(in-package :defperson)

(defvar *system-dir*
  (asdf:system-source-directory "defperson"))

(defvar *font-dir*
  (merge-pathnames "fonts/" *system-dir*))

(defvar *default-font-file*
  (merge-pathnames (make-pathname :type "ttf") *font-dir*))

(defun font-file (name)
  (merge-pathnames name *default-font-file*))

;;; these are TTF conversions of lmmono{9,10,12}-regular.otf
(pdf:load-ttf-font (font-file "LMMono9-Regular"))
(pdf:load-ttf-font (font-file "LMMono10-Regular"))
(pdf:load-ttf-font (font-file "LMMono12-Regular"))

(defun draw-string (string)
  (pdf:set-rgb-fill (/ 188.0 255.0) (/ 143.0 255.0) (/ 143.0 255.0))
  (pdf:draw-text (format nil "~S" string)))

(defun draw-escaped-symbol (string)
  (pdf:set-gray-fill 0.0)
  (pdf:draw-text (format nil "|~A|" string)))

(defun draw-indented-keyword-on-next-line (keyword)
  (pdf:set-rgb-fill (/ 218.0 255.0) (/ 112.0 255.0) (/ 214.0 255.0))
  (pdf:move-to-next-line)
  (pdf:draw-text (format nil "  ~(~S~) " keyword)))

(defun draw-indented-comment-on-next-line (keyword)
  (pdf:set-rgb-fill (/ 178.0 256.0) (/ 34.0 256.0) (/ 34.0 256.0))
  (pdf:move-to-next-line)
  (pdf:draw-text (format nil "  ;; ~(~S~)" keyword)))

(defun mm->pt (mm)
  (* mm 72 0.039378497))

(defparameter *people*
  '(("Jānis Džeriņš" "Goldsmiths, University of London" "http://doc.gold.ac.uk/~mas01cr/" #*1111 (:speaker :local-organization))
    ("Christophe Rhodes" "Goldsmiths, University of London" "http://doc.gold.ac.uk/~mas01cr/" #*1111 (:speaker :local-organization))))

(defparameter *ids* nil)

(defparameter *boxesp* nil)

(defvar *venue*
  "#| ELS 2015, April 20-21, Goldsmiths, UK |#")

(defun draw-box (x y)
  (pdf:rectangle (+ (* x (+ (mm->pt 85.0) (mm->pt 10.0))) (mm->pt 15.0))
                 (+ (* y (mm->pt 54.0)) (mm->pt 13.5))
                 (mm->pt 85.0)
                 (mm->pt 54.0))
  (pdf:stroke))

(defun cards (&key (people *people*) (ids *ids*) (boxesp *boxesp*)
                (venue *venue*))
  (let ((*venue* venue))
    (loop for i from 0
       until (null people)
       do (pdf:with-document ()
            (pdf:with-page ()
              (pdf:set-gray-stroke 0.0)
              (pdf:set-line-width 0.0)
              (dotimes (x 2)
                (dotimes (y 5)
                  (when boxesp
                    (draw-box x y))
                  (if (null people)
                      (blank-person x y)
                      (apply #'person x y (pop people))))))
            (pdf:write-document (format nil "/tmp/cards~2,'0D.pdf" i))))
    (loop for i from 0 until (null ids)
       do (pdf:with-document ()
            (pdf:with-page ()
              (pdf:set-gray-stroke 0.0)
              (pdf:set-line-width 0.0)
              (dotimes (x 2)
                (dotimes (y 5)
                  (when boxesp
                    (draw-box x y))
                  (unless (null ids)
                    (apply #'id x y (pop ids))))))
            (pdf:write-document (format nil "/tmp/ids~2,'0D.pdf" i))))))

(defun blank-cards (&optional (boxesp *boxesp*))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:set-gray-stroke 0.0)
      (pdf:set-line-width 0.0)
      (dotimes (x 2)
        (dotimes (y 5)
          (when boxesp
            (draw-box x y))
          (blank-person x y))))
    (pdf:write-document (format nil "/tmp/cards-blank.pdf"))))

(defun draw-organization (organization)
  (cond
    (organization
     (draw-indented-keyword-on-next-line :organization)
     (cond
       ((> (length organization) 61) (error "organization too long"))
       ((> (length organization) 41)
        (let ((break (position #\Space organization :start 20 :end 38)))
          (unless break (error "not enough spaces in organization"))
          (pdf:move-to-next-line)
          (pdf:draw-text "  ")
          (pdf:set-rgb-fill (/ 188.0 255.0) (/ 143.0 255.0) (/ 143.0 255.0))
          (pdf:draw-text (format nil "\"~A ~~" (subseq organization 0 break)))
          (pdf:move-to-next-line)
          (pdf:draw-text "   ")
          (pdf:draw-text (format nil "~A\"" (subseq organization (1+ break))))))
       ((> (length organization) 21)
        (pdf:move-to-next-line)
        (pdf:draw-text "  ")
        (draw-string organization))
       (t (draw-string organization))))
    (t (draw-indented-comment-on-next-line :organization))))

(defun draw-email (url &optional (font (pdf:get-font "LMMono10-Regular")))
  (cond
    (url
     (draw-indented-keyword-on-next-line :email)
     (pdf:set-gray-fill 0.0)
     (pdf:draw-text "#e\"")
     (pdf:set-rgb-fill 0.0 0.0 (/ #xee 255.0))
     (let (;; 10 11pt characters fit in 2.1cm; we have 5.87cm to play with
           (size (min 11.0 (* 11.0 (/ 26 (length url))))))
       #|(* 11.0 (/ 58.7 (* 2.1 (length url)))))))|#
       (pdf:set-font font size)
       (pdf:draw-text url))
     (pdf:set-gray-fill 0.0)
     (pdf:set-font font 11.0)
     (pdf:draw-text "\""))
    (t (draw-indented-comment-on-next-line :email))))

;;; not used in ELS 2015
(defun draw-url (url &optional (font (pdf:get-font "LMMono10-Regular")))
  (cond
    (url
     (draw-indented-keyword-on-next-line :url)
     (pdf:set-gray-fill 0.0)
     (pdf:draw-text "#u\"")
     (pdf:set-rgb-fill 0.0 0.0 (/ #xee 255.0))
     (let (;; 10 11pt characters fit in 2.1cm; we have 5.87cm to play with
           (size (min 11.0 (* 11.0 (/ 28 (length url))))))
       #|(* 11.0 (/ 58.7 (* 2.1 (length url)))))))|#
       (pdf:set-font font size)
       (pdf:draw-text url))
     (pdf:set-gray-fill 0.0)
     (pdf:set-font font 11.0)
     (pdf:draw-text "\""))
    (t (draw-indented-comment-on-next-line :url))))

(defun draw-footer (x y font)
  (pdf:in-text-mode
    (pdf:move-text (+ (* x (+ (mm->pt 85.0) (mm->pt 10.0))) 
                      (mm->pt 3.0)
                      (mm->pt 15.0))
                   (+ (* y (mm->pt 54.0)) (mm->pt 13.5) (mm->pt 3.0)))
    (pdf:set-rgb-fill (/ 178.0 256.0) (/ 34.0 256.0) (/ 34.0 256.0))
    (pdf:set-font font 9.0)
    (pdf:draw-text *venue*)))

(defun blank-person (x y)
  (let* ((font9 (pdf:get-font "LMMono9-Regular"))
         (font10 (pdf:get-font "LMMono10-Regular"))
         (font12 (pdf:get-font "LMMono12-Regular")))
    (pdf:set-gray-stroke 0.0)
    (pdf:in-text-mode
      (pdf:set-font font10 11.0)
      (pdf:move-text (+ (* x (+ (mm->pt 85.0) (mm->pt 10.0))) 
                        (mm->pt 15.0)
                        (mm->pt 3.0))
                     (+ (* y (mm->pt 54.0)) (mm->pt 13.5) (mm->pt 54.0) -25.0))
      (pdf:set-text-leading 13.0)
      (pdf:set-gray-fill 0.0)
      (pdf:draw-text "(")
      (pdf:set-rgb-fill (/ 160.0 255.0) (/ 32.0 255.0) (/ 240.0 255.0))
      (pdf:draw-text "defperson ")
      (draw-indented-keyword-on-next-line :organization)
      (pdf:move-to-next-line)
      (draw-indented-keyword-on-next-line :email)
      (pdf:move-to-next-line)
      (draw-indented-keyword-on-next-line :banquet)
      (pdf:set-gray-fill 0.0)
      (pdf:draw-text (format nil "      )")))
    (draw-footer x y font9)))

(defun person (x y name organization email banquet features &rest plist)
  (let* ((font9 (pdf:get-font "LMMono9-Regular"))
         (font10 (pdf:get-font "LMMono10-Regular"))
         (font12 (pdf:get-font "LMMono12-Regular")))
    (pdf:set-gray-stroke 0.0)
    (pdf:in-text-mode
      (pdf:set-font font10 11.0)
      (pdf:move-text (+ (* x (+ (mm->pt 85.0) (mm->pt 10.0))) 
                        (mm->pt 15.0)
                        (mm->pt 3.0))
                     (+ (* y (mm->pt 54.0)) (mm->pt 13.5) (mm->pt 54.0) -25.0))
      (pdf:set-text-leading 13.0)
      (pdf:set-gray-fill 0.0)
      (pdf:draw-text "(")
      (pdf:set-rgb-fill (/ 160.0 255.0) (/ 32.0 255.0) (/ 240.0 255.0))
      (pdf:draw-text "defperson ")
      (let ((size (min 24.0 (* 24.0 (/ 13.5 (length name))))))
        (pdf:set-text-leading (+ size 5))
        (pdf:move-to-next-line)
        (pdf:set-text-rise 4.0)
        (pdf:draw-text "    ")
        (pdf:set-font font12 size))
      (draw-escaped-symbol name)
      (pdf:set-font font10 11.0)
      (pdf:set-text-leading 13.0)
      (pdf:set-text-rise 0.0)
      (when (or organization email) 
        (draw-organization organization)
        (draw-email email))
      (draw-indented-keyword-on-next-line :banquet)
      (pdf:set-gray-fill 0.0)
      (pdf:draw-text (format nil "~(~S~)" banquet))
      (when plist
        (loop for (key value) on plist by #'cddr
              do (draw-indented-keyword-on-next-line key)
                 (pdf:set-gray-fill 0.0)
                 (pdf:draw-text (format nil "~(~S~)" value))))
      (when features
        (draw-indented-keyword-on-next-line :features)
        (pdf:set-gray-fill 0.0)
        (let* ((*print-case* :downcase)
               (*print-right-margin* 33)
               (*print-pretty* t)
               (text (format nil "'~:<~@{~W~^ ~:_~}~:>" features))
               (length (length text)))
          (cond
            ((< length 25)
             (assert (null (position #\Newline text)))
             (pdf:draw-text text))
            (t
             (pdf:move-to-next-line)
             (pdf:draw-text "  ")
             (loop for start = 0 then (1+ nlpos)
                   for nlpos = (position #\Newline text) then (position #\Newline text :start start)
                   do (pdf:draw-text (subseq text start nlpos))
                   when (null nlpos) do (return)
                   do (pdf:move-to-next-line) (pdf:draw-text "  "))))))
      (pdf:draw-text ")"))
    (draw-footer x y font9)))

(defun id (x y username password &rest ignore)
  (let* ((font9 (pdf:get-font "LMMono9-Regular"))
         (font10 (pdf:get-font "LMMono10-Regular"))
         (font12 (pdf:get-font "LMMono12-Regular")))
    (pdf:set-gray-stroke 0.0)
    (pdf:in-text-mode
      (pdf:set-font font9 9.0)
      (pdf:move-text (+ (* x (+ (mm->pt 85.0) (mm->pt 10.0))) 
                        (mm->pt 15.0)
                        (mm->pt 12.0))
                     (+ (* y (mm->pt 54.0)) (mm->pt 13.5) (mm->pt 54.0) -65.0))
      (pdf:set-text-leading 11.0)
      (pdf:set-gray-fill (/ 32.0 255.0))
      (pdf:draw-text "username: ")
      (pdf:set-gray-fill 0.0)
      (pdf:draw-text username)
      (pdf:move-to-next-line)
      (pdf:set-gray-fill (/ 32.0 255.0))
      (pdf:draw-text "password: ")
      (pdf:set-gray-fill 0.0)
      (pdf:draw-text password))
    (draw-footer x y font9)))

#+nil ; convenience
(defun read-people ()
  (let ((list (fare-csv:read-csv-file "/home/csr21/goldsmiths/research/els2015/registrations/registrations.csv")))
    (flet ((convert (line)
             (declare (optimize debug))
             (list (first line)
                   (and (string/= (third line) "") (third line))
                   (and (string/= (second line) "") (second line))
                   (string= (ninth line) "1")
                   (let (features)
                     (when (string= (nth 14 line) "1") (push :author features))
                     (when (string= (nth 15 line) "1") (push :demonstrator features))
                     (when (string= (nth 16 line) "1") (push :programme features))
                     (when (string= (nth 17 line) "1") (push :local features))
                     (when (string= (nth 18 line) "1") (push :invited features))
                     (when (string= (nth 19 line) "1") (push :steering features))
                     features))))
      (mapcar #'convert (cdr list)))))

#+nil ; convenience
(defun read-ids ()
  (fare-csv:read-csv-file "/home/csr21/goldsmiths/research/els2015/registrations/eduroam.txt"))

#+nil ; do this to make name tags and WiFi id/password PDFs
(cards (read-people) (read-ids))
