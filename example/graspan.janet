# A program analysis example, to get the data set see:
#  - https://github.com/Graspan/graspan-cpp
# which links to:
# - https://drive.google.com/drive/folders/0B8bQanV_QfNkbDJsOWc2WWk4SkE?usp=sharing
# We run against the httpd_df data set.

(use ../datafog)

(var nodes @[])
(var edges @[])

(print "loading row data...")
(var data 
  (->> "./httpd_df"
       (slurp)
       (string/split "\n")
       (filter |(not (string/has-prefix? "#" $)))
       (filter |(not (empty? $)))
       (map |(string/split "\t" $))
       (map |[(scan-number ($ 0)) (scan-number ($ 1)) (keyword ($ 2))])))

(print "building node and edge facts...")
(each [src dest t] data
  (case t
    :e
    (array/push edges [src dest])
    :n
    (array/push nodes [dest src])))

(set data nil)

(print "loading edges...")
(set edges (new-variable edges))
(print "loading nodes...")
(set nodes (new-variable nodes))

(print "running query...")
(while (some truthy? (map variable-update-and-changed? [nodes edges]))
  (print "joining data...")
  # N(a,c) <-  N(a,b), E(b,c)
  (join-into-variable nodes nodes edges |[$2 $1]))

(print "collecting results...")
(set nodes (new-relation ;(nodes :stable)))

(pp (length nodes))
