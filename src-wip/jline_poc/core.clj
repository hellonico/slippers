(ns jline-poc.core
  (:require [pyjama.core])
  (:import
    [org.jline.reader LineReader LineReaderBuilder]
    [org.jline.reader.impl DefaultParser]
    [org.jline.reader.impl.history DefaultHistory]
    [org.jline.terminal TerminalBuilder]
    [org.jline.reader History History$Entry]                     ; for History$Entry
    [java.nio.file Paths]))

(defn echo [input]
  (pyjama.core/call {:stream true :model "llama3.1" :prompt input}))

(defn show-help []
  (println "Available commands:")
  (println "  :help                 - Show this help message")
  (println "  :quit                 - Exit the program")
  (println "  :history [n]          - Show last n history entries (default 20)")
  (println "  !n                    - Re-run history entry with index n")
  (println "  <text>                - Echo back your input")
  (println)
  (println "History tips:")
  (println "  Up/Down or Ctrl-P/Ctrl-N to navigate history")
  (println "  Ctrl-R                Reverse search history (i-search)")
  (println "  Enter                 Run the selected line"))

(defn print-history
  ([^History history] (print-history history 20))
  ([^History history n]
   (let [n (max 1 (int n))
         size (.size history)
         start (max 0 (- size n))]
     (doseq [i (range start size)]
       (let [^History$Entry e (.get history i)]
         (println (format "%d  %s" (.index e) (.line e))))))))

(defn run-history-index [^History history idx]
  (let [idx (int idx)]
    (when (or (neg? idx) (>= idx (.size history)))
      (throw (ex-info (str "No history entry at index " idx) {:idx idx})))
    (let [^History$Entry e (.get history idx)
          cmd (.line e)]
      (println (str "! " cmd))
      cmd)))

(defn -main []
  (let [terminal (-> (TerminalBuilder/builder) (.system true) .build)
        parser   (doto (DefaultParser.)
                   ;; correct JLine API:
                   (.setEofOnUnclosedQuote false))
        history  (DefaultHistory.)
        reader   (-> (LineReaderBuilder/builder)
                     (.terminal terminal)
                     (.parser parser)
                     (.history history)
                     .build)
        home     (System/getProperty "user.home")
        histfile (Paths/get home (into-array String [".jline-poc.history"]))]

    ;; Persist history & tune size
    (doto reader
      (.setVariable LineReader/HISTORY_FILE histfile)
      (.setVariable LineReader/HISTORY_SIZE (int 2000))
      ;; optional niceties:
      (.setVariable "history-ignore-duplicates" true)
      (.setVariable "history-ignore-space" true))

    (println "Welcome to Pyjama/JLine POC! Type :help for commands.")
    (try
      (loop []
        (let [line (.readLine reader "\n>>> ")]
          (cond
            (nil? line) (recur)

            (or (= line ":quit") (= line "exit"))
            (println "Goodbye!")

            (= line ":help")
            (do (show-help) (recur))

            (re-matches #":history(?:\s+(\d+))?" line)
            (let [m (re-matches #":history(?:\s+(\d+))?" line)
                  n (some-> (second m) Integer/parseInt)]
              (print-history history (or n 20))
              (recur))

            (re-matches #"!(\d+)" line)
            (let [idx (Integer/parseInt (second (re-matches #"!(\d+)" line)))
                  expanded (try
                             (run-history-index history idx)
                             (catch Exception e
                               (println (.getMessage e))
                               nil))]
              (when expanded
                (echo expanded))
              (recur))

            (empty? line)
            (recur)

            :else
            (do (echo line) (recur)))))
      (finally
        (.save history)))))
