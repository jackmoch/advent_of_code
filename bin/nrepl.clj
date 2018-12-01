(ns bin.nrepl
  (:require [nrepl.server :as srvr]))

(defonce s (srvr/start-server :port 9999))

(spit (doto (java.io.File. ".nrepl-port")
        (.deleteOnExit))
      (:port s))

(println (str "REPL started on port " (:port s)))
