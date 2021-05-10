# Some basics for an educational janet datalog engine.
#
# - We have a concept known as a variable, which is simply a list of tuples.
# - Each tuple in a variable's list corresponds to a datalog fact.
# - Each N arity variable stores the data in N sorted arrays.
# - The ith array is sorted by the value at the ith tuple index.
# - The arrays act as indicies that allow us to do joins between variables.
# - As a special case, the 0th array is also sorted by all elements so we can
#   do efficient searches for any given fact.
# - To avoid duplicating work, each variable is divided into three sections:
#    - :to-add are facts pending addition to a variable.
#    - :recent are facts just added to the variable that have yet to be processed.
#    - :stable are facts that have been in the variable for a while.
# - :stable facts are divided into sub groups so small recent additions don't trigger
#   a large resort of all our indicies.

(defn gallop
  "Gallop search through sorted data until f is false, returning that index.
  This can be used for efficiently scanning a large list for a given key."
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

(defn new-variable
  "Create a new variable for storing facts."
  [arity facts]
  (def facts (distinct facts))
  (def partitioned-facts @[])
  (for i 0 arity
    (array/push partitioned-facts
                @{:stable @[]
                  :recent @[]}))
  @{:facts partitioned-facts
    :arity arity
    :to-add @[facts]})

(defn variable-add-facts
  [v facts]
  (array/push (v :to-add) facts))

(defn- join-helper
  "Call (f ltup rtup) on each element of the join of two lists of fact tuples.
   The join occurs between elements at lidx and ridx for each entry in l and r.
   The function requires the fact lists are deduplicated and sorted on the respective indicies."
  [l lidx r ridx f]
  (var i 0)
  (var j 0)
  (while (and (< i (length l))
              (< j (length r)))
    (def lk ((l i) lidx))
    (def rk ((r j) ridx))
    (case (cmp lk rk)
      -1
      (set i (gallop l i |(< ($ lidx) rk)))
      1
      (set j (gallop r j |(< ($ ridx) lk)))
      0
      (do
        (var lcount 0)
        (var rcount 0)
        (while (= lk (get-in l [(+ lcount i) lidx]))
          (++ lcount))
        (while (= rk (get-in r [(+ rcount j) ridx]))
          (++ rcount))
        (for x 0 lcount
          (for y 0 rcount
            (f (l (+ i x)) (r (+ j y)))))
        (set i (+ i lcount))
        (set j (+ j rcount))))))

(defn join-variables
  "Join two variables' recent facts adding (xform ljoin rjoin) to the return result."
  [l lidx r ridx xform]
  (def l ((l :facts) lidx))
  (def r ((r :facts) ridx))
  (def results @[])
  (def on-join |(array/push results (xform $0 $1)))
  (each batch1 (l :stable)
    (join-helper batch1 lidx (r :recent) ridx on-join))
  (each batch2 (r :stable)
    (join-helper (l :recent) lidx batch2 ridx on-join))
  (join-helper (l :recent) lidx (r :recent) ridx on-join)
  results)

(defn join-variables-into
  "Join two variables' recent facts adding (xform ljoin rjoin) to the return result."
  [v l lidx r ridx xform]
  (def to-add (join-variables l lidx r ridx xform))
  (unless (empty? to-add)
    (array/push (v :to-add) to-add)))

(defn summarize-variable
  [v]
  (def to-add (get v :to-add))
  (def recent (get-in v [:facts 0 :recent]))
  (def stable (get-in v [:facts 0 :stable]))
  (sort
    (distinct
      (array/concat @[] to-add recent ;stable))))

(defn variable-update-and-changed?
  "Perform an iteration step update, returning true if there was a change in the variable."
  [v]
  (for i 0 (v :arity)
    (var recent (get-in v [:facts i :recent]))
    (unless (empty? recent)
      # Merge recent into stable.
      (put-in v [:facts i :recent] @[])
      (def stable (get-in v [:facts i :stable]))
      # Collapse stable facts into large sorted lists gradually as facts arrive.
      (while (and (not (empty? stable))
                  (<= (length (last stable)) (* 2 (length recent))))
        (def batch (array/pop stable))
        (array/concat batch recent)
        (def deduped (distinct batch))
        (set recent
             (if (= 0 i)
               (sort deduped)
               (sort-by |(get $ i) deduped))))
      (array/push stable recent)))
  (var to-add (v :to-add))
  (unless (empty? to-add)
    (put v :to-add @[])
    (set to-add (distinct (array/concat @[] ;to-add)))
    (each batch (get-in v [:facts 0 :stable])
      (set to-add (filter |(not (gallop-contains? batch $)) to-add)))
    (for i 0 (v :arity)
      (def recent
        (if (= 0 i)
          (sorted to-add)
          (sorted-by |(get $ i) to-add)))
      (put-in v [:facts i :recent] recent)))
  (not (empty? (get-in v [:facts 0 :recent]))))


(def parent
  (new-variable 2 [["dad" "son"]
                   ["gramps" "dad"]
                   ["great-gramps" "gramps"]]))
(def ancestor (new-variable 2 []))

(while (some truthy? (map variable-update-and-changed? [parent ancestor]))
  # ancestor(x, y) <- parent(x, y) 
  (join-variables-into ancestor parent 0 parent 0 (fn [l r] l))

  # ancestor(x, z) <- ancestor(x, y), ancestor(y, z) 
  (join-variables-into ancestor ancestor 1 ancestor 0 (fn [l r] [(l 0) (r 1)])))

(pp (summarize-variable ancestor))
