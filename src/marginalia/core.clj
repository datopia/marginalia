;; ## A new way to think about programs
;;
;; What if your code and its documentation were one and the same?
;;
;; Much of the philosophy guiding literate programming is the realization of the answer to this question.
;; However, if literate programming stands as a comprehensive programming methodology at one of end of the
;; spectrum and no documentation stands as its antithesis, then Marginalia falls somewhere between. That is,
;; you should always aim for comprehensive documentation, but the shortest path to a useful subset is the
;; commented source code itself.
;;
;; ## The art of Marginalia
;;
;; If you’re fervently writing code that is heavily documented, then using Marginalia for your Clojure projects
;; is as simple as running it on your codebase. However, if you’re unaccustomed to documenting your source, then
;; the guidelines herein will help you make the most out of Marginalia for true-power documentation.
;;
;; Following the guidelines will work to make your code not only easier to follow – it will make it better.
;; The very process of using Marginalia will help to crystalize your understanding of problem and its solution(s).
;;
;; The quality of the prose in your documentation will often reflect the quality of the code itself thus highlighting
;; problem areas. The elimination of problem areas will solidify your code and its accompanying prose. Marginalia
;; provides a virtuous circle spiraling inward toward maximal code quality.
;;
;; ## The one true way
;;
;; 1. Start by running Marginalia against your code
;; 2. Cringe at the sad state of your code commentary
;; 3. Add docstrings and code comments as appropriate
;; 4. Generate the documentation again
;; 5. Read the resulting documentation
;; 6. Make changes to code and documentation so that the “dialog” flows sensibly
;; 7. Repeat from step #4 until complete
;;
(ns marginalia.core
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]
            [marginalia.html :refer [uberdoc-html index-html single-page-html]]
            [marginalia.parser :as parser
             :refer [parse-file parse-ns]]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))


(def ^{:dynamic true} *test* "src/marginalia/core.clj")
(def ^{:dynamic true} *comment* #"^\s*;;\s?")

;; ## File System Utilities

(defn- ensure-directory!
  "Ensure that the directory specified by `file` exists.  If not then make it so.
   Here is a snowman ☃"
  [file]
  (when-not (.exists file)
    (.mkdirs file)))

(defn- dir? [file]
  (.isDirectory file))

(defn find-file-extension
  "Returns a string containing the files extension."
  [^java.io.File file]
  (second (re-find #"\.([^.]+)$" (.getName file))))

(defn processable-file?
  "Predicate. Returns true for \"normal\" files with a file extension which
  passes the provided predicate."
  [pred ^java.io.File file]
  (when (.isFile file)
    (-> file find-file-extension pred)))

(defn find-processable-file-paths
  "Returns a seq of processable file paths (strings) in alphabetical order by
  namespace."
  [dir pred]
  (->> (io/file dir)
       file-seq
       (filter  (partial processable-file? pred))
       (sort-by (comp second parse-ns))
       (map     #(.getCanonicalPath %))
       not-empty))

;; ## Project Info Parsing
;; Marginalia will parse info out of your project.clj to display in
;; the generated html file's header.


(defn parse-project-form
  "Parses a project.clj file and returns a map in the following form

       {:name
        :version
        :dependencies
        :dev-dependencies
        etc...}
  by merging into the name and version information the rest of the defproject
  forms (`:dependencies`, etc)"
  [[_ project-name version-number & attributes]]
  (merge {:name    (str project-name)
          :version version-number}
         (apply hash-map attributes)))

(defn parse-project-file
  "Parses a project file -- './project.clj' by default -- and returns a map
   assembled according to the logic in parse-project-form."
  ([]
   (parse-project-file "./project.clj"))
  ([path]
   (try
     (with-open [rdr (java.io.PushbackReader. (io/reader path))]
       (loop [form (read rdr)]
         (if (= 'defproject (first form))
           (parse-project-form form)
           (recur (read rdr)))))
     (catch Exception e
       nil))))

(defn path-to-doc [fn]
  {:ns     (parse-ns (io/file fn))
   :groups (parse-file fn)})

;; ## Output Generation

(defn filename-contents
  [props output-dir all-files parsed-file]
  {:name     (io/file output-dir (str (:ns parsed-file) ".html"))
   :contents (single-page-html props parsed-file all-files)})

(defn multidoc!
  [output-dir files-to-analyze props]
  (let [parsed-files (map path-to-doc files-to-analyze)
        index        (index-html props parsed-files)
        pages        (for [f parsed-files]
                       (filename-contents props output-dir parsed-files f))]
    (doseq [f (conj pages {:name     (io/file output-dir "toc.html")
                           :contents index})]
      (spit (:name f) (:contents f)))))

(defn uberdoc!
  "Generates an uberdoc html file from 3 pieces of information:

   2. The path to spit the result (`output-file-name`)
   1. Results from processing source files (`path-to-doc`)
   3. Project metadata as a map, containing at a minimum the following:
     - :name
     - :version
  "
  [output-file-name files-to-analyze props]
  (let [source (uberdoc-html
                props
                (map path-to-doc files-to-analyze))]
    (spit output-file-name source)))

;; ## External Interface (command-line, lein, cake, etc)

;; These functions support Marginalia's use by client software or command-line
;; users.

(def ^:private file-extensions #{"clj" "cljs" "cljx" "cljc"})

(defn find-sources
  "Given a collection of filepaths, returns a lazy sequence of filepaths to all
   .clj, .cljs, .cljx, and .cljc files on those paths: directory paths will be searched
   recursively for files."
  [sources]
  (if (empty? sources)
    (find-processable-file-paths "./src" file-extensions)
    (->> sources
         (mapcat #(if (dir? %)
                    (find-processable-file-paths % file-extensions)
                    [(.getCanonicalPath (io/file %))]))
         not-empty)))

(defn- source-excluded?
  "Check if a source file is excluded from the generated documentation"
  [source opts]
  (loop [exclusions (:exclude opts)]
    (let [[h & exclusions] exclusions]
      (cond (not h)                         false
            (re-find (re-pattern h) source) true
            :else                           (recur exclusions)))))

(defn- vec-assoc-fn [m k v]
  (update m k (fnil conj []) v))

(def ^:private cli-options
  [["-d" "--dir"
    "Directory into which the documentation will be written"
    :default "./docs"]

   ["-f" "--file"
    "File into which the documentation will be written"
    :default "uberdoc.html"]

   ["-n" "--name NAME"
    "Project name."]

   ["-v" "--version VERSION"
    "Project version."]

   ["-D" "--desc DESCRIPTION"
    "Project description."
    :id :description]

   ["-a" "--dep DEPEDENDENCY"
    "Project dependency of the form <group>:<artifact>."
    :assoc-fn vec-assoc-fn]

   ["-c" "--css RESOURCE-OR-URI"
    "Additional CSS resource name or external URI."
    :assoc-fn vec-assoc-fn]

   ["-j" "--js RESOURCE-OR-URI"
    "Additional JS resources name or external URI."
    :assoc-fn vec-assoc-fn
    :id       :javascript]

   ["-m" "--multi"
    "Generate each namespace documentation as a separate file"
    :id :multi?]

   ["-e" "--exclude SOURCE-FILE"
    "Exclude source file(s) from the document generation process."
    :assoc-fn vec-assoc-fn]

   ["-L" "--lift-inline-comments"
    "Lift comments to the top of the enclosing form. They will be
    treated as if they preceded the enclosing form."
    :default true
    :id      :lift-inline?]

   ["-X" "--exclude-lifted-comments"
    "If comments are being lifted into documentation then also exclude
     them from the source code display."
    :default true
    :id      :exclude-lifted?]

   ["-h" "--help"
    :id :help?]])

(def ^:private usage-preamble        "Options:\n")
(def ^:private arg-mismatch-preamble "Wrong number of arguments passed to Marginalia.\n")

(defn- exclude-sources [sources opts]
  (for [s sources :when (not (source-excluded? s opts))]
    s))

(let [proj-keys #{:name :version :description :dependencies}]
  (defn- merge-opts [opts proj]
    (-> proj
        (merge (select-keys opts proj-keys))
        (update :marginalia merge opts))))

(defn -main
  "Default generation: given a collection of filepaths in a project, find the .clj
   files at these paths and, if Clojure source files are found:

   1. Print out a message to std out letting a user know which files are to be processed;
   1. Create the docs directory inside the project folder if it doesn't already exist;
   1. Call the uberdoc! function to generate the output file at its default location,
     using the found source files and a project file expected to be in its default location.

   If no source files are found, complain with a usage message."
  [& args]
  (let [parsed (parse-opts args cli-options)
        opts   (:options parsed)]
    (if (:help? opts)
      (println (str usage-preamble (:summary parsed)))
      (if-let [sources (find-sources (:arguments parsed))]
        (let [proj    (merge-opts opts (parse-project-file))
              opts    (:marginalia proj)
              sources (into [] (exclude-sources sources opts))]
          (ensure-directory! (:dir opts))
          (binding [parser/*lift-inline-comments*   (:lift-inline?    opts)
                    parser/*delete-lifted-comments* (:exclude-inline? opts)]
            (if (:multi? opts)
              (multidoc! (:dir opts)                sources proj)
              (uberdoc!  (str (:dir opts) "/" file) sources proj))))
        (println (str arg-mismatch-preamble (:summary parsed)))))))
