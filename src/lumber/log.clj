(ns lumber.log
  (:require
   [clojure.string :as str]
   [lumber.util :as u]))

(defonce logger (atom (agent nil)))

(def ^:dynamic *preamble* nil)
(defmacro with-preamble [preamble & body]
  `(binding [*preamble* ~preamble]
     ~@body))
(defn preamble [] *preamble*)

(defn printer [config level ns & args]
  (apply println
         (u/format-timestamp (System/currentTimeMillis)
                             "yyyy-MM-dd hh:mm:ss")
         (str (-> level name str/upper-case)
              (when *preamble*
                (str " " *preamble*)))
         (str ns ":" (:line config))
         (->> args
              (map
               (fn [x]
                 (if (string? x)
                   x
                   (u/safe-pr-str x)))))))

(def ^:dynamic *level* (atom :info))
(defn set-level!
  "Force the current binding of log level to new level."
  [level]
  (reset! *level* level))
(defmacro with-level [level & body]
  `(binding [*level* (atom ~level)]
     ~@body))

(def level-map
  {:fatal #{:fatal}
   :error #{:error :fatal}
   :warn  #{:warn :error :fatal}
   :info  #{:info :warn :error :fatal}
   :debug #{:debug :info :warn :error :fatal}
   :trace #{:trace :debug :info :warn :error :fatal}})

(defmacro log* [config level & rest]
  `(when (contains? (get level-map @*level*) ~level)
     (printer ~config ~level ~(str *ns*) ~@rest)))

(defmacro log [level & rest]
  `(log* ~(meta &form) ~level ~@rest))

(defmacro deflogfn [level & [log-fn]]
  `(defmacro ~(symbol (name level)) [& args#]
     (let [log-fn# (or ~log-fn 'log*)
           level# ~level
           fmeta# ~(meta &form)]
       `(~log-fn# ~fmeta# ~level# ~@args#))))

(deflogfn :fatal)
(deflogfn :error)
(deflogfn :warn)
(deflogfn :info)
(deflogfn :debug)
(deflogfn :trace)

(defmacro log-errors [& body]
  `(try
     ~@body
     (catch Throwable ex#
       (error (.getMessage ex#)))))

(defmacro log-and-rethrow-errors [& body]
  `(try
     ~@body
     (catch Throwable ex#
       (error (.getMessage ex#))
       (throw ex#))))

(defmacro spy
  ([v]
   `(spy :debug ~v))
  ([level v]
   `(let [v# ~v]
      (log* ~(meta &form) ~level '~v v#)
      v#)))
