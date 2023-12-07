(defparameter test-input-1
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(defun split (seq delim)
  (loop for i = 0 then (1+ j)
        as j = (position delim seq :start i)
        collect (subseq seq i j)
        while j))

(defun stringify (x) (format nil "~a" x))

(defun seek (x seq &keys from-end)
  (let ((index (search (stringify x) seq :from-end from-end)))
    (if (and index from-end)
        (- (length seq) index 1)
        index)))

(defun seek-digit (line &keys from-end)
  ;; Return the head
  (caar
   (sort
    (remove-if
     ;; Remove all missing searches
     (lambda (x) (null (third x)))
     ;; Index all digits from start of line
     (mapcar
      (lambda (x)
        (list (car x) (cadr x) (seek (cadr x) line :from-end from-end)))
      ;; For part 1, just remove all the word mappings
      '((1 "one") (1 1)
        (2 "two") (2 2)
        (3 "three") (3 3)
        (4 "four") (4 4)
        (5 "five") (5 5)
        (6 "six") (6 6)
        (7 "seven") (7 7)
        (8 "eight") (8 8)
        (9 "nine") (9 9)
        (0 "zero") (0 0))))
    ;; Sort by smallest index first
    (lambda (a b) (> (third b) (third a))))))

(loop for line in (split test-input-1 #\Newline)
      for x = (seek-digit line :from-end nil)
      for y = (seek-digit line :from-end t)
      sum (parse-integer (format nil "~a~a" x y)))

(loop for line in (load-input "day1.input")
      for x = (seek-digit line :from-end nil)
      for y = (seek-digit line :from-end t)
      sum (parse-integer (format nil "~a~a" x y)))

(defun load-input (fname)
  (with-open-file (in fname)
    (loop for line = (read-line in nil)
          while line
          collect line)))
