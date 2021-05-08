(defn gallop
  "Gallop search through sorted data until f is false, returning that index."
  [ind i f]
  (var step 1)
  (var i i)
  (while true
    (def i+step (+ i step))
    (unless (and (< i+step (length ind))
                 (f (ind i+step)))
      (break))
    (set i i+step)
    (set step (blshift step 1)))
  (set step (brshift step 1))
  (while (< 0 step)
    (def i+step (+ i step))
    (when (and (< i+step (length ind))
               (f (ind i+step)))
      (set i i+step))
    (set step (brshift step 1)))
  (inc i))

(defn gallop-contains?
  "Check if v is part of a sorted list using gallop to search."
  [ind v]
  (let [i (gallop ind 0 |(<= $ v))]
    (= v (get ind (dec i)))))

(defn join-helper
  "Given two tuple relations, call (f (t1 0) (t1 1) (t2 2)) on each join on (t1/t2 0)."
  [l r f]
  (var i 0)
  (var j 0)
  (while (and (< i (length l))
              (< j (length r)))
    (def li0 ((l i) 0))
    (def rj0 ((r j) 0))
    (case (cmp li0 rj0)
      -1
      (set i (gallop l i |(< ($ 0) rj0)))
      0
      (do
        (var lcount 0)
        (var rcount 0)
        (while (= li0 (get-in l [(+ lcount i) 0]))
          (++ lcount))
        (while (= rj0 (get-in r [(+ rcount j) 0]))
          (++ rcount))
        (for x 0 lcount
          (for y 0 rcount
            (f li0 ((l (+ i x)) 1) ((r (+ j y)) 1))))
        (set i (+ i lcount))
        (set j (+ j rcount)))
      1
      (set j (gallop r j |(< ($ 0) li0))))))

(defn new-relation
  [& relations]
  (sort (distinct (array/concat @[] ;relations))))

(defn merge-into-relation
  [r & relations]
  (sort (distinct (array/concat r ;relations))))

(defn new-variable
  [& relations]
  @{:stable @[]
    :recent @[]
    :to-add @[(new-relation ;relations)]})

(defn join-into-variable
  "Join two variables adding (xform k v1 v2) as the out variable."
  [out input1 input2 xform]
  (def results @[])
  (def on-join |(array/push results (xform $0 $1 $2)))
  (each batch1 (input1 :stable)
    (join-helper (input2 :recent) batch1 on-join))
  (each batch2 (input2 :stable)
    (join-helper (input1 :recent) batch2 on-join))
  (join-helper (input1 :recent) (input2 :recent) on-join)
  (unless (empty? results)
    (array/push (out :to-add) results))
  out)

(defn variable-update-and-changed?
  "Perform an iteration step update, returning true if there was a change."
  [v]
  (unless (empty? (v :recent))
    # Merge recent into stable.
    (def recent (v :recent))
    (put v :recent @[])
    (while (and (not (empty? (v :stable)))
                (<= (length (last (v :stable))) (* 2 (length recent))))
      (merge-into-relation recent (array/pop (v :stable))))
    (array/push (v :stable) recent))
  (unless (empty? (v :to-add))
    # set recent to to-add minus stable 
    (var to-add (new-relation ;(v :to-add)))
    (put v :to-add @[])
    (each batch (v :stable)
      (set to-add (filter |(not (gallop-contains? batch $)) to-add)))
    (put v :recent to-add))
  (not (empty? (v :recent))))