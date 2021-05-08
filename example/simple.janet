(use ../datafog)

(def eav (new-variable [["andrew" [:talks-to "sogaiu"]]]))
(def query (new-variable [["andrew" '?result]]))
(def result (new-variable))

(while (some truthy? (map variable-update-and-changed? [query result eav]))
  (join-into-variable result eav query (fn [k a b] [k a])))

(pp result)