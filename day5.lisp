;; day 5

(defun load-input (fname)
  (with-open-file (stream fname)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split (seq delim)
  (loop for i = 0 then (1+ j)
        as j = (position delim seq :start i :test 'equal)
        collect (subseq seq i j)
        while j))

(defparameter test-input-1
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defstruct interval x1 y1 range)
(defun parse-interval (line)
  (let ((xs (mapcar #'parse-integer (split line #\ ))))
    (make-interval
     :x1 (nth 1 xs)
     :y1 (nth 0 xs)
     :range (nth 2 xs))))

(defstruct mapping from to intervals)

(defun parse-mapping (raw-mapping)
  (let ((key (first (split (first raw-mapping) #\Space))))
    (make-mapping
     :from (first (split key #\-))
     :to (car (last (split key #\-)))
     :intervals (loop for line in (rest raw-mapping)
                      collect (parse-interval line)))))

(defun apply-mapping (x mapping)
  (loop for interval in (mapping-intervals mapping)
        for x1 = (interval-x1 interval)
        for y1 = (interval-y1 interval)
        for range = (interval-range interval)
        for x2 = (+ x1 range)
        when (and (<= x1 x) (<= x x2))
          return (+ y1 (- x x1))
        finally (return x)))

(defparameter test-seeds-2
  "seeds: 3169137700 271717609 3522125441 23376095 1233948799 811833837 280549587 703867355 166086528 44766996 2326968141 69162222 2698492851 14603069 2755327667 348999531 2600461189 92332846 1054656969 169099767")
(defstruct seed-interval x1 range)
(defun parse-seeds (line)
  (loop for (s1 s2) on (split (string-trim '(#\Space) (second (split line #\:))) #\Space) by #'cddr
        collect (make-seed-interval
                 :x1 (parse-integer s1)
                 :range (parse-integer s2))))

(defstruct almanac seeds mappings)
(defun parse-almanac (input)
  (let ((defs (split input "")))
    (make-almanac
     :seeds (parse-seeds (caar defs))
     :mappings (mapcar 'parse-mapping (rest defs)))))

(parse-almanac (split test-input-1 #\Newline))

;; Part 2 why tf it not working???
(let ((almanac (parse-almanac (split test-input-1 #\Newline))))
  (loop for s in (almanac-seeds almanac)
        for lo = (seed-interval-x1 s)
        for hi = (+ lo (seed-interval-range s))
        minimizing (loop for x from lo to hi
                         return
                         (loop for mapping in (almanac-mappings almanac)
                               for input = x then output
                               as output = (apply-mapping input mapping)
                               finally (return output)))))
