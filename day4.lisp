;; day 4

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
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defstruct card winners actual (wins 0) (copies 1))

(defun parse-card (raw-nums)
  (mapcar #'parse-integer
          (remove-if
           (lambda (xs) (= 0 (length xs)))
           (split
            (string-trim '(#\Space) raw-nums)
            #\Space))))

(count-wins
 (parse-line  (first (split test-input-1 #\Newline))))

(defun parse-line (line)
  (let* ((nums  (split (cadr (split line #\:)) #\|))
         (expected (parse-card (first nums)))
         (actual   (parse-card (second nums))))
    (make-card :winners expected
               :actual  actual)))

(defun count-wins (card)
  (loop with wins = 0
        for num in (card-actual card)
        when (find num (card-winners card))
          do (incf wins)
        finally (return wins)))

;; Part 1
(loop for line in (load-input "day4.input.txt")
      for card = (parse-line line)
      for wins = (count-wins card)
      when (> wins 0)
        sum (expt 2 (1- wins)))

;; Part 2

(defun get-cards (lines)
  (loop for line in lines
        for card = (parse-line line)
        do (setf (card-wins card) (count-wins card))
        collect card))

;; potentially n^2 but who cares
(defun process-cards (cards)
  (loop for card in cards
        for i = 1 then (1+ i)
        for copies = (card-copies card)
        for wins = (card-wins card)
        when (> wins 0)
          do (loop for card in (subseq cards i (+ i wins))
                   do (incf (card-copies card) copies)))
  (loop for card in cards
        sum (card-copies card)))

(process-cards (get-cards (load-input "day4.input.txt")))
