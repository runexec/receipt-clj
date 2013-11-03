(ns receipt.core
  (:gen-class)
  (:require [hiccup.core :as hc]
            [garden.core :as gd]))

(def ^:dynamic *css*)
(def ^{:dynamic true :doc "default value doc/"} *doc-dir* "doc/")
(def ^{:dynamic true :doc "holds namespace seq"}  *namespaces*)
(def ^{:dynamic true 
       :doc "controls the look of the html. theme-light fn is default"}
  *builder*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fns that provide for finders

(defn- filter-ns 
  "Retrieves all vars from a ns"
  [ns pred]
  (->> ns
       ns-interns
       vals
       (filter #(-> % meta pred))
       seq))

(defn- private? [meta] (true? (:private meta)))
(defn- dynamic? [meta] (true? (:dynamic meta)))
(defn- declared? [meta] (true? (:declared meta)))
(defn- macro? [meta] (true? (:macro meta)))

(defn- -fn? [meta] 
  (and 
   (:arglists meta)
   (not (macro? meta))))

(defn- variable? [meta]
  (not 
   (= true 
      (or (macro? meta)
          (-fn? meta)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Finders

(defn macros 
  "returns all macros in namespace"
  [-ns]
  (filter-ns -ns macro?))


(defn functions 
  "returns all functions in namespace"
  [-ns]
  (filter-ns -ns -fn?))

(defn variables 
  "returns all non-dynamic variables in namespace"
  [-ns]
  (filter-ns -ns variable?))

(defn declared
  "returns all declared in namespace"
  [-ns]
  (filter-ns -ns declared?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Builders

(defn- var->str 
  "Takes a variable and makes it html safe string"
  [v]
  (-> v str (clojure.string/split #"/") last hc/h))

(defn- vars->css-map
  "Returns returns a map with the var being the key and the value
   being a css of all matching class attributes."
  [coll]
  (let [-css (atom "doc")
        class! #(swap! -css str %)]
    (apply 
     merge
     (for [c coll :let [m (meta c)]]
       (do 
         (reset! -css "doc")
         (if (private? m) (class! " private"))
         (if (dynamic? m) (class! " dynamic"))
         (if (macro? m) (class! " macro"))
         (if (-fn? m) (class! " function"))
         (if (declared? m) (class! " declared"))
         (if (variable? m) (class! " variable"))
         {c (clojure.string/triml @-css)})))))

(defn- var->css-map 
  "Single value version of vars->css-map"
  [v]
  (first (vars->css-map [v])))

(defn- vars->args-hiccup 
  "Takes a variable and returns a html safe version of the argslists meta"
  [coll]
  (for [c coll 
        :let [m (meta c)
              d (:doc m)
              args (:arglists m)
              c (var->str c)]]
    {:doc (hc/h d)
     :args (map
            #(into [] [:p %])
            (for [a args] 
              (->> (apply list c a)
                   (interpose " ")
                   (reduce str)
                   (format "(%s)"))))}))

(defn- var->args-hiccup 
  "Single value version of vars->args-hiccup"
  [v]
  (first (vars->args-hiccup [v])))

(defn- css-str->hiccup-element
  "Converts css string to <a class=\"attribute\">attribute</a>"
  [s]
  (let [s (-> s str hc/h)]
    [:a {:class (str "attribute " s)} s]))

(defn- var->hiccup-header 
  "html safe header for a variables"
  [v]
  (let [vname (var->str v)]
    [:div {:class :header :name vname :id vname} vname]))

(defn- build-var 
  "Responsible for the format of each var's HTML display"
  [v]
  (let [header (var->hiccup-header v)
        css (-> v var->css-map last)
        tags (-> css (clojure.string/split #" ") rest)
        tags (interpose " " (map css-str->hiccup-element tags))
        v-name (var->str v)
        {:keys [doc args]} (var->args-hiccup v)]
    {:sort (str v)
     :link [:li [:a {:href (str "#" v-name)} v-name]]
     :hiccup (into [:div
                    (into header 
                          [(hc/html [:span {:class "tags"} tags])])
                    "\n"
                    (when-not (empty? doc)
                      [:pre {:class "docstring"} doc])]
                   args)}))

(defn- build-namespace 
  "Controls the final layout of each namespace"
  [-ns] 
  (let [d (map build-var (declared -ns))
        v (map build-var (variables -ns))
        f (map build-var (functions -ns))
        m (map build-var (macros -ns))
        items (->> (merge m f v d)
                   (filter seq)
                   flatten
                   (sort-by :sort))
        body (map :hiccup items)
        links (map :link items)
        nspaces (map #(do [:li [:a {:href (str % ".html")} (str %)]])
                     *namespaces*)]
    (hc/html 
     [:html
      [:head
       [:style *css*]]
      [:body


       [:h1 (format "(ns %s)" -ns)]

       [:table
        [:tr
         [:td {:class :left}
          [:table {:class :navigation}
           [:tr
            [:td {:class :left}
             [:ul 
              [:li [:a {:href :#}[:b [:u "Namespaces"]]]]
              nspaces]]
            [:td
             [:ul 
              [:li [:a {:href :#}[:b [:u "API"]]]]
              links]]]]]
         [:td
          [:div {:class "body"} body]]]]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CSS

(defn theme-light 
  "Default builder theme"
  [-ns]
  (binding [*css* 
            (apply 
             str
             (map gd/css 
                  [[:li {:margin :10px :list-style :none}]
                   [:a:link {:font-weight :bold
                             :color :#F9F9F9
                             :text-decoration :none}]
                   [:a:visited {:font-weight :bold
                                :color :#F9F9F9
                                :text-decoration :none}]
                   [:a:hover {:font-weight :bold
                                :color :#B9D9C2
                                :text-decoration :none}]
                   [:.navigation {:background-color :#2F2933}]
                   [:.left {:float :left}]
                   [:h1 {:text-align :center}]
                   [:body {:background-color :#F9F9F9}]
                   [:.body {:margin "50px 10px 20px 10px"
                            :width :600px}] 
                   [:.docstring {:background-color :#FFFFFF
                                 :padding :10px
                                 :margin :20px
                                 :border-color :#2F2933
                                 :border-width "1px 0 1px 0"
                                 :border-style :solid}]
                   [:.tags {:background-color :#FFFFFF
                            :padding-left :40px}]
                   [:.private {:border-color :#FF1200
                               :background-color :#FFFFFF
                               :border-style :dashed
                               :border-width "0 4px 0 4px"
                               :padding "0 15px 0 15px"}]
                   [:.dynamic {:border-color :#08A400
                               :background-color :#FFFFFF
                               :border-style :dashed
                               :border-width "0 4px 0 4px"
                               :padding "0 15px 0 15px"}]
                   [:.variable {:border-color :#A47A00
                                :background-color :#FFFFFF
                                :border-style :dashed
                                :border-width "0 4px 0 4px"
                                :padding "0 15px 0 15px"}]
                   [:.function {:border-color :#A40094
                                :background-color :#FFFFFF
                                :border-style :dashed
                                :border-width "0 4px 0 4px"
                                :padding "0 15px 0 15px"}]
                   [:.macro {:border-color :#FFE900
                             :background-color :#FFFFFF
                             :border-style :dashed
                             :border-width "0 4px 0 4px"
                             :padding "0 15px 0 15px"}]
                   [:.declared {:border-color :#000000
                                :background-color :#FFFFFF
                                :border-style :dashed
                                :border-width "0 4px 0 4px"
                                :padding "0 15px 0 15px"}]
                   [:.header {:border-color :#B9D9C2 
                              :background-color :#FFFFFF
                              :border-style :solid
                              :border-width "0 0 2px 0"
                              :color :#2F2933 
                              :font-weight :bold
                              :font-size :large
                              :margin "0 0 10px 0"
                              :padding "10px 0 5px 10px"}]]))]
    (build-namespace -ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Namespace and project handling

(defn- read-source-path 
  "looks for lein project file and loads source paths. default config path is project.clj"
  [& [config-path]]
  (binding [*read-eval* false]
    (let [paths (->> (or config-path "project.clj") 
                     slurp 
                     read-string
                     (drop-while #(not= % :source-paths))
                     second)
          paths (or paths ["src/"])]
      paths)))

(defn- path->ns
  "source-path is removed from the path which becomes the ns"
  [source-path path]
  (let [path (.. path
                 (replace source-path "")
                 (replace "//" "/")
                 (replace "/" ".")
                 (replace "_" "-")
                 (replace ".clj" ""))
        path (if (not= (first path) ".")
               path
               (apply str (rest path)))]
    (symbol path)))

(defn- path->ns-coll 
  "Takes a source path and runs path->ns recursively"
  [src-path]
  (->> src-path 
       clojure.java.io/file 
       file-seq 
       (map (memfn getPath))
       (filter #(.. % (endsWith ".clj")))
       (map #(path->ns src-path %))
       sort))

(defn- namespaces
  "looks for lein project file and loads source paths. default config path is project.clj"
  [& [config-path]]
  (flatten
   (map path->ns-coll 
        (if-not config-path 
          (read-source-path)
          (read-source-path config-path)))))

(defn- ns->save-path 
  "Output file name"
  [ns] 
  (str ns ".html"))

(defn- ns-save-file-coll 
  "looks for lein project file and loads source paths. default config path is project.clj"
  [& [config-path]]
 (let [coll (if-not config-path 
              (namespaces)
              (namespaces config-path))]
   (partition-all
    2
    (interleave
     coll
     (map ns->save-path coll)))))

(defn- ensure-doc-path! 
  "Test if *doc-dir* exists else create"
  []
  (let [fp (clojure.java.io/file *doc-dir*)]
    (if-not (.exists fp)
      (.mkdir fp))))
  
(defn- generate-html!
  "
  Looks for lein project file and loads source paths. default config path is project.clj
  The path binding of *doc-dir* will be created (mkdir) if it doesn't exists.
  The form binding of *builder* will be called with the ns as the first argument.
  "
  [& [config-path]]
  (ensure-doc-path!)
  (let [coll (if-not config-path 
               (ns-save-file-coll)
               (ns-save-file-coll config-path))
        d (.. (str *doc-dir* "/") (replace "//" "/"))
        nspaces (->> coll flatten (filter symbol?))]
    (binding [*namespaces* nspaces]
      (doseq [[-ns filename] coll
              :let [filename (str d filename)]]
        (require [-ns])
        (println "Writing" filename)
        (spit filename (*builder* -ns))))))

(defn -main
  "Saves docs to doc-dir path. TODO add support for the custom themes."
  ([] (-main *doc-dir*))
  ([doc-dir] (-main doc-dir :light))
  ([doc-dir theme-or-css-path]
     (let [theme (case (keyword theme-or-css-path)                                      
                   :light theme-light
                   (-> "Invalid theme." Exception. throw))]
       (binding [*doc-dir* doc-dir
                 *builder* theme]
         (generate-html!)))))

