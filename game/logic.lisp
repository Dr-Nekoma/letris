(in-package #:letris)

(defun rotate-piece-left (piece)
  (let ((rep (representation piece)))
    (destructuring-bind (m n) (array-dimensions rep)
      (loop :for i :below (- m 1)
            :do (loop :for j :from i :below (- n i 1)
                      :do (rotatef (aref rep i j) (aref rep j (- m i 1))
                                   (aref rep (- m i 1) (- n j 1)) (aref rep (- m j 1) i)))))
    piece))

(defun rotate-piece-right (piece)
  (let ((rep (representation piece)))
    (destructuring-bind (m n) (array-dimensions rep)
      (loop :for i :below (- m 1)
            :do (loop :for j :from i :below (- n i 1)
                      :do (rotatef (aref rep j (- m i 1)) (aref rep i j)
                                   (aref rep (- m j 1) i) (aref rep (- m i 1) (- n j 1))))))

    piece))

(defun has-collision (board piece)
  (declare (optimize (debug 3) (safety 3)))
  (destructuring-bind (piece-row-size piece-column-size) (array-dimensions (representation piece))
    (destructuring-bind (board-row-size board-column-size) (array-dimensions board)
      (let* ((initial-position (pos piece))
             (piece-pos-x (first initial-position))
             (piece-pos-y (second initial-position))
             (piece-matrix (representation piece))
             (answer :no-collision))
        (loop :for i :below piece-row-size
              :do (loop :for j :below piece-column-size
                        :do (let* ((piece-value (aref piece-matrix i j))
                                   (board-x (+ i piece-pos-x))
                                   (board-y (+ j piece-pos-y))
                                   (is-piece-value (/= piece-value 0)))
			      (when is-piece-value
				(let ((side-collision (or (>= board-y board-column-size) (< board-y 0)))
				      (bottom-collision (>= board-x board-row-size)))
				  (if (or side-collision bottom-collision)
				      (progn
					(when side-collision
					  (setf answer :side-collision))
					(when bottom-collision
					  (setf answer :bottom-collision)))
				      (let ((board-value (aref board board-x board-y)))
					(when (/= board-value 0)
					  (setf answer :board-collision)))))))))
        answer))))

(defun spawn-random-piece ()
  (let ((index (random 6)))
    (case index
      (0 (spawn 't-piece))
      (1 (spawn 's-piece))
      (2 (spawn 'i-piece))
      (3 (spawn 'j-piece))
      (4 (spawn 'o-piece))
      (5 (spawn 'z-piece)))))

(defun find-row-to-clear (board)
  (destructuring-bind (h w) (array-dimensions board)
    (loop :for i :below (* h w) :by w
          :for row := (make-array w :displaced-to board
                                    :displaced-index-offset i)
          :when (every (lambda (x) (/= x 0)) row)
          :do (return (floor i w)))))

(defun clear-row (board index)
  (let* ((h (array-dimension board 0))
         (w (array-dimension board 1))
         (flat-board (make-array (* h w)
                                 :displaced-to board))
         (top-part (make-array (* w index)
                               :displaced-to board)))
    (setf (subseq flat-board w (* w (+ 1 index))) top-part)))

(defun check-board (board)
  (loop :for ind := (find-row-to-clear board)
        :while ind
        :count (clear-row board ind)))

(defun move-adjustments (piece board func)
  (let* ((new-piece (funcall func (copy piece)))
	(collision-result (has-collision board new-piece)))
    (when (eql collision-result :no-collision)
      (setf (pos piece) (pos new-piece)
	    (representation piece) (representation new-piece)))
    collision-result))

(defun change-coords (coords)
  (lambda (piece)
    (setf (pos piece) coords)
    piece))

(defun reach-insta-collision (board piece)
  (let ((result (handle-input board piece :s)))
    (or (eql :bottom-collision result) (eql :board-collision result))))

(defun insta-place (board)
  (lambda (piece)
    (loop :while (not (reach-insta-collision (car board) piece)))
    piece)) 

(defun handle-pause (board)
  (with-slots (paused current-button) board
    (if paused
	(progn
	  (setf paused nil)
	  (setf current-button :s)
	  :idle)
	(progn
	  (setf paused t)
	  :paused))))

(defun handle-input (board piece movement)
  (with-slots (board-representation paused) board
    (let ((x (first (pos piece)))
	  (y (second (pos piece))))
      (case movement
	(:d     (move-adjustments piece board-representation (change-coords `(,x ,(+ y 1)))))
	(:a     (move-adjustments piece board-representation (change-coords `(,x ,(- y 1)))))
	(:q     (move-adjustments piece board-representation  #'rotate-piece-left))
	(:e     (move-adjustments piece board-representation  #'rotate-piece-right))
	;(:w    (TODO: Save and/or swap piece with piece already saved))
	(:s     (move-adjustments piece board-representation (change-coords `(,(+ x 1) ,y))))
	(:space (move-adjustments piece board-representation (insta-place `(,board))))
	(:p     (handle-pause board))
	(otherwise (if paused :paused :idle))))))

(defun glue-piece-on-board (board piece)
  (let ((init-pos-x (first (pos piece)))
        (init-pos-y (second (pos piece)))
        (dimension (array-dimension (representation piece) 0))
        (piece-matrix (representation piece)))
    (loop :for i :from init-pos-x :below (+ init-pos-x dimension)
          :for a :from 0 :below dimension
          :do (loop :for j :from init-pos-y :below (+ init-pos-y dimension)
                    :for b :from 0 :below dimension
                    :do (when (array-in-bounds-p board i j)
                          (let ((board-value (aref board i j))
                                (piece-value (aref piece-matrix a b)))
                            (setf (aref board i j) (logior board-value piece-value))))))))

(defun give-score (lines &optional (level 0))
  (* (+ 1 level)
     (ecase lines
       (0 0)
       (1 40)
       (2 100)
       (3 300)
       (4 1200))))

(defun level-up (level lines-counter lines-cleared)
  (let ((sum-lines (+ lines-cleared lines-counter))
	(threshold (* level 5)))
    (if (>= sum-lines threshold)
	(values (- sum-lines threshold) (+ 1 level))
	(values lines-counter level))))
