
(asdf:operate 'asdf:load-op :database-querying)

(in-package :database-querying)

(activate-monitor fcg::trace-fcg-database-querying)

;; (query-database "which cars are hybrid")
;; (query-database "how many cars are hybrid")