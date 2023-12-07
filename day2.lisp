;; day 2

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
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defstruct rgb r g b)

(defun parse-rgb (raw)
  (flet
      ((count-color (color defs)
         (parse-integer
          (or (car (find-if (lambda (x) (string= color (second x))) defs))
              "0"))))
    (let ((defs
            (mapcar
             (lambda (x) (split (string-trim '(#\Space) x) #\Space))
             (split raw #\,))))
      (make-rgb
       :r (count-color "red" defs)
       :g (count-color "green" defs)
       :b (count-color "blue" defs)))))


(defun parse-game (line)
  "Parse a game into a list of RGBs"
  (mapcar
   'parse-rgb
   (split
    (second (split line #\:))
    #\;)))

(defun test-game (game)
  (every
   (lambda (x) (and (<= (rgb-r x) 12)
                    (<= (rgb-g x) 13)
                    (<= (rgb-b x) 14)))
   game))

(defun maximize-rgb (game)
  (* (loop for rgb in game
              maximizing (rgb-r rgb))
        (loop for rgb in game
              maximizing (rgb-g rgb))
        (loop for rgb in game
              maximizing (rgb-b rgb)))
  )

(loop for line in (split test-input-1 #\Newline)
      for id = 1 then (1+ id)
      for game = (parse-game line)
      sum (maximize-rgb game))

(loop for line in (load-input "day2.input")
      for id = 1 then (1+ id)
      for game = (parse-game line)
      sum (maximize-rgb game))
