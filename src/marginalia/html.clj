(ns marginalia.html
  "Utilities for converting parse results into html."
  (:require [hiccup.core :refer [html]]
            [hiccup.util :refer [escape-html]])
  (:import [com.petebevin.markdown MarkdownProcessor]))

(def ^{:dynamic true} *resources* "./vendor/")

(defn css-rule [rule]
  (let [sels (reverse (rest (reverse rule)))
        props (last rule)]
    (str (apply str (interpose " " (map name sels)))
         "{" (apply str (map #(str (name (key %)) ":" (val %) ";") props)) "}")))

(defn css
  "Quick and dirty dsl for inline css rules, similar to hiccup.

   ex. `(css [:h1 {:color \"blue\"}] [:div.content p {:text-indent \"1em\"}])`

   -> `h1 {color: blue;} div.content p {text-indent: 1em;}`"
  [& rules]
  (html [:style {:type "text/css"}
         (apply str (map css-rule rules))]))

(defn slurp-resource
  "Stolen from leiningen"
  [resource-name]
  (try
    (-> (.getContextClassLoader (Thread/currentThread))
        (.getResourceAsStream resource-name)
        (java.io.InputStreamReader.)
        (slurp))
    (catch java.lang.NullPointerException npe
      (println (str "Could not locate resources at " resource-name))
      (println "    ... attempting to fix.")
      (let [resource-name (str *resources* resource-name)]
        (try
          (-> (.getContextClassLoader (Thread/currentThread))
              (.getResourceAsStream resource-name)
              (java.io.InputStreamReader.)
              (slurp))
          (catch java.lang.NullPointerException npe
            (println (str "    STILL could not locate resources at " resource-name ". Giving up!"))))))))

(defn inline-js [resource]
  (let [src (slurp-resource resource)]
    (html [:script {:type "text/javascript"}
           src])))

(defn inline-css [resource]
  (let [src (slurp-resource resource)]
    (html [:style {:type "text/css"}
           (slurp-resource resource)])))




;; The following functions handle preparation of doc text (both comment and docstring
;; based) for display through html & css.

;; Markdown processor.
(let [mdp (com.petebevin.markdown.MarkdownProcessor.)]
  (defn md
    "Markdown string to html converter. Translates strings like:

   \"# header!\" -> `\"<h1>header!</h1>\"`

   \"## header!\" -> `\"<h2>header!</h2>\"`

   ..."
    [s]
    (.markdown mdp s)))

;; As a result of docifying then grouping, you'll end up with a seq like this one:
;; <pre><code>[...
;; {:docs [{:docs-text "Some doc text"}]
;;  :codes [{:code-text "(def something \"hi\")"}]}
;; ...]</code></pre>
;;
;; `docs-to-html` and `code-to-html` convert their respective entries into html,
;; and `group-to-html` calls them on each seq item to do so.

(defn docs-to-html
  "Converts a docs section to html by threading each doc line through the forms
   outlined above.

   ex. (docs-to-html [{:doc-text \"# hello world!\"} {:docstring-text \"I'm a docstring!}])

   ->  `\"<h1>hello world!</h1><br />\"`
   "
  [docs]
  (-> docs
      str
      (md)))

(defn code-to-html [code-block]
  (html [:pre.clojure
         (escape-html code-block)]))

(defn section-to-html [section]
  (html [:div {:class "section"}
         [:div {:class "docs"} (docs-to-html
                                (if (= (:type section) :comment)
                                  (:raw section)
                                  (:docstring section)))]
         [:div {:class "code"} (if (= (:type section) :code)
                                 (code-to-html (:raw section))
                                 "")]]))

(defn dependencies-html [deps & header-name]
  (when-let [deps (seq deps)]
    (let [header-name (or header-name "dependencies")]
      (html [:div {:class "dependencies"}
             [:h3 header-name]
             [:table
              (map #(html [:tr
                           [:td {:class "dep-name"} (str (first %))]
                           [:td {:class "dotted"} [:hr]]
                           [:td {:class "dep-version"} (second %)]])
                   deps)]]))))

;; # Load Optional Resources
;; Use external Javascript and CSS in your documentation. For example:
;; To format Latex math equations, download the
;; [MathJax](http://www.mathjax.org/) Javascript library to the docs
;; directory and then add
;;
;;     :marginalia {:javascript ["mathjax/MathJax.js"]}
;;
;; to project.clj. Below is a simple example of both inline and block
;; formatted equations.
;;
;; Optionally, you can put the MathJax CDN URL directly as a value of `:javascript`
;; like this:
;;
;;     :marginalia {
;;       :javascript
;;         ["http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"]}
;;
;; That way you won't have to download and carry around the MathJax library.
;;
;; When \\(a \ne 0\\), there are two solutions to \\(ax^2 + bx + c = 0\\) and they are
;; $$x = {-b \pm \sqrt{b^2-4ac} \over 2a}.$$

(defn opt-resources-html
  "Generate script and link tags for optional external javascript and css."
  [project-info]
  (let [options    (:marginalia project-info)
        javascript (:javascript options)
        css        (:css options)]
    (html (concat
           (when javascript
             (map #(vector :script {:type "text/javascript" :src %}) javascript))
           (when css
             (map #(vector :link {:type "text/css" :rel "stylesheet" :href %}) css))))))

;; Is &lt;h1/&gt; overloaded?  Maybe we should consider redistributing
;; header numbers instead of adding classes to all the h1 tags.
(defn header-html [project-info]
  (html
   [:div {:class "docs"}
    [:div {:class "header"}
     [:h1 {:class "project-name"} (if (seq (:url project-info))
                                    [:a {:href (:url project-info)} (:name project-info)]
                                    (:name project-info))]
     [:h2 {:class "project-version"} (:version project-info)]
     [:br]
     (md (:description project-info))]
    (dependencies-html (:dependencies project-info))
    (dependencies-html (:dev-dependencies project-info) "dev dependencies")]))

(defn link-to-namespace
  "Creates an 'a' tag pointing to the `namespace-name`, either as an anchor (if
  `anchor?` is true) or as a link to a separate `$namespace-name.html` file.
  If `attrs` aren't empty, they are added to the resulting tag."
  [namespace-name anchor? & attrs]
  [:a (into {:href (if anchor?
                     (str "#" namespace-name)
                     (str namespace-name ".html"))}
        attrs)
   namespace-name])

(defn link-to-toc
  "This is a hack, as in the case when `anchor?` is false, the link will contain
  a reference to `toc.html` which might not even exist."
  [anchor?]
  (link-to-namespace "toc" anchor? {:class "toc-link"}))

(defn toc-html [props docs]
  (html
   [:div.docs
    [:div.toc
     [:a {:name "toc"} [:h3 "namespaces"]]
     [:ul
      (map #(vector :li (link-to-namespace (:ns %) (:uberdoc? props)))
           docs)]]]))

(defn floating-toc-html [docs]
  [:div {:id "floating-toc"}
   [:ul
    (map #(vector :li {:class "floating-toc-li"
                       :id (str "floating-toc_" (:ns %))}
                  (:ns %))
         docs)]])

(defn groups-html [props doc]
  (html
   [:div.docs
    [:div.docs-header
     [:a.anchor {:name (:ns doc) :href (str "#" (:ns doc))}
      [:h1.project-name
       (:ns doc)]
      (link-to-toc (:uberdoc? props))]]]

   (map section-to-html (:groups doc))))

(defn page-template
  [project-metadata opt-resources header toc content floating-toc]
  (html
   "<!DOCTYPE html>\n"
   [:html
    [:head
     [:meta {:http-equiv "Content-Type" :content "text/html" :charset "utf-8"}]
     [:meta {:name "description" :content (:description project-metadata)}]
     opt-resources
     [:title (:name project-metadata) " -- Marginalia"]]
    [:body
     header
     toc
     content
     [:div {:class "footer"}
      "Generated by "
      [:a {:href "https://github.com/datopia/marginalia"} "Marginalia"]
      floating-toc]]]))


(defn uberdoc-html
  "This generates a stand alone html file (think `lein uberjar`).
   It's probably the only var consumers will use."
  [project-metadata docs]
  (page-template
   project-metadata
   (opt-resources-html project-metadata)
   (header-html project-metadata)
   (toc-html {:uberdoc? true} docs)
   (map #(groups-html {:uberdoc? true} %) docs)
   (floating-toc-html docs)))

(defn index-html
  [project-metadata docs]
  (page-template
   project-metadata
   (opt-resources-html project-metadata)
   (header-html project-metadata)
   (toc-html {:uberdoc? false} docs)
   ""   ;; no contents
   "")) ;; no floating toc

(defn single-page-html
  [project-metadata doc all-docs]
  (page-template
   project-metadata
   (opt-resources-html project-metadata)
   "" ;; no header
   "" ;; no toc
   (groups-html {:uberdoc? false} doc)
   "" ;; no floating toc
   ))
