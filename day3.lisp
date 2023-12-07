;; day3.lisp

(defun load-input (fname)
  (with-open-file (stream fname)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split (seq delim)
  (loop for i = 0 then (1+ j)
        as j = (position delim seq :start i)
        collect (subseq seq i j)
        while j))

(defparameter test-input-1
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defun coord (x y) (format nil "~a,~a" x y))
(defun locate-symbols (grid)
  "Returns a hash map of symbol coordinates in the grid"
  (setf locs (make-hash-table :test 'equal))

  (loop for line in grid
        for y = 0 then (1+ y)
        do (loop for char across line
                 for x = 0 then (1+ x)
                 ;; If it's not "." or a digit, it's a symbol
                 when (and (not (char= #\. char))
                           (not (digit-char-p char)))
                   do (setf (gethash (coord x y) locs) char)))

  locs)

(defstruct num y x1 x2 value)
(defun string-integer-p (str)
  (handler-case (progn (parse-integer str) t)
    (parse-error nil)))
(defun locate-nums (grid)
  "Returns a list of position-aware numbers"
  (setq seq '())
  (loop for line in grid
        for y = 0 then (1+ y)
        do (loop for num in (split
                             (substitute-if #\.
                                            (lambda (x) (not (digit-char-p x)))
                              line)
                              #\.)
                 for x1 = 0 then (1+ x2)
                 as x2 = (+ x1 (length num))
                 when (string-integer-p num)
                   do (push  (make-num :x1 x1
                                       :x2 (- x2 1)
                                       :y y
                                       :value (parse-integer num))
                             seq)))
  (nreverse seq))

(locate-nums (split test-input-1 #\Newline))

(defun near-symbol (num locs)
  (loop for y from (- (num-y num) 1) to (1+ (num-y num))
        when (loop
               for x from (- (num-x1 num) 1) to (1+ (num-x2 num))
               for coord = (coord x y)
               when (gethash coord locs)
                 return t)
          return t))

(defparameter test-input-2
  "12.......*..
+.........34
.......-12..
..78........
..*....60...
78.........9
.5.....23..$
8...90*12...
............
2.2......12.
.*.........*
1.1..503+.56")

(loop with grid = (load-input "day3.input")
      with locs = (locate-symbols grid)
      with nums = (locate-nums grid)
      for num in nums
      when (near-symbol num locs)
        sum (num-value num))

(defun get-gear (num locs)
  (loop for y from (- (num-y num) 1) to (1+ (num-y num))
        for coord =
                  (loop
                    for x from (- (num-x1 num) 1) to (1+ (num-x2 num))
                    for coord = (coord x y)
                    for value = (gethash coord locs)
                    when (and value (char= #\* value))
                      return coord)
        when coord return coord))
(progn
  (setf gears (make-hash-table :test 'equal))
  (loop with grid = (load-input "day3.input")
        with locs = (locate-symbols grid)
        with nums = (locate-nums grid)
        for num in nums
        for gear-loc = (get-gear num locs)
        when gear-loc
          do (let ((values (or '() (gethash gear-loc gears))))
               (push (num-value num) values)
               (setf (gethash gear-loc gears) values)))
  (loop for nums being the hash-value of gears
        when (= (length nums) 2)
          sum (apply #'* nums)))
