(ns com.climate.newrelic.trace)

(defn- traced-meta [fname]
  {'com.newrelic.api.agent.Trace {:metricName (str *ns* \. fname)
                                  :dispatcher true}})

(defn ^:private not-ampersand?
  "Returns true if the argument is not an ampersand symbol."
  [x]
  (not= x '&))

(defn ^:private without-ampersands
  "Filters out all ampersands from the vector.  Returns a vector."
  [coll]
  (filterv not-ampersand? coll))

(defn ^:private restructure-args
  "Returns an argument list that removes all destructuring."
  [args]
  (mapv (fn [arg]
          (cond
            (map? arg) (gensym "map_arg_")
            (vector? arg) (gensym "vector_arg_")
            (symbol? arg) arg
            :default (throw "Oh no!")))
        args))

(defn ^:private process-destructuring
  "Takes a raw list of arguments and returns a vector of two elements.  The
  first element is a vector of arguments that eliminates all destructuring.
  The second element is a sequence of the destructurings and their respective
  restructured names.

  For example, given the argument list [[a b] c & {:keys [foo]}], this function
  will return something like:

    [[vector_arg_062 c & map_arg_073]
     [[a b] vector_arg_062 {:keys [foo]} map_arg_073]]"
  [args]
  (let [restructured-args (restructure-args args)
        destructure-mappings (->> (map vector args restructured-args)
                                  (filter (fn [[x y]] (not= x y)))
                                  (apply concat))]
    [restructured-args destructure-mappings]))

(defn ^:private preproc-decl
  "Processes the body of a function declaration, returning a vector that
  contains the function metadata and a sequence of maps containing information
  about function arguments and the function bodies."
  [fname fdecl]
  ; mostly stolen from clojure.core/defn
  (let [; get docstring
        m (if (string? (first fdecl))
            {:doc (first fdecl)}
            {})
        fdecl (if (string? (first fdecl))
                (next fdecl)
                fdecl)
        ; look for meta at beginning
        m (if (map? (first fdecl))
            (conj m (first fdecl))
            m)
        fdecl (if (map? (first fdecl))
                (next fdecl)
                fdecl)
        ; allow for single-arity functions
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        ; look for meta at end
        m (if (map? (last fdecl))
            (conj m (last fdecl))
            m)
        fdecl (if (map? (last fdecl))
                (butlast fdecl)
                fdecl)
        ann-fdecl (for [[raw-args & body] fdecl
                        :let [[args arg-mappings] (process-destructuring raw-args)]]
                    {:raw-args raw-args
                     :args args
                     :no-amp-args (without-ampersands args)
                     :arg-mappings arg-mappings
                     :body body})]
    [m ann-fdecl]))

(defmacro defn-traced
  "Drop-in replacement for clojure.core/defn.

  Tells Newrelic that entry/exit to this function should be traced.
  Allows time spent in this function to be tracked as a % of total
  request time.

  If one function defined with defn-traced calls another defined with
  defn-traced, Newrelic will correctly show both in its transaction
  trace and show how much time was spent inside/outside the inner
  function."
  [fname & fdecl]
  (let [[m ann-fdecl] (preproc-decl fname fdecl)
        tname (gensym (str fname "_Tracer_"))
        iname (gensym (str fname "_TracerInterface_"))
        oname (gensym (str fname "_tracer_instance_"))]
    `(do
       (def ~fname)
       (definterface ~iname
         ~@(for [{:keys [no-amp-args]} ann-fdecl]
           `(~'invoke [~@ no-amp-args])))
       (deftype ~tname []
         ~iname
         ~@(for [{:keys [no-amp-args arg-mappings body]} ann-fdecl]
             (if (seq arg-mappings)
               `(~'invoke [~'_ ~@no-amp-args]
                  (let [~@arg-mappings] ~@body))
               `(~'invoke [~'_ ~@no-amp-args] ~@body))))
       (let [~oname (new ~tname)]
         (defn ~fname
           ~m
           ~@(for [{:keys [args no-amp-args]} ann-fdecl]
               `(~args (.invoke ~oname ~@no-amp-args))))))))
