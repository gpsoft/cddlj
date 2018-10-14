(in-ns 'user)

(require '[mount.core :as mount]
         '[clojure.tools.namespace.repl :as tn])

(defn start [] (mount/start))
(defn go [] (mount/start) :ready)
(defn reset
  []
  (mount/stop)
  (tn/refresh :after 'user/go))
