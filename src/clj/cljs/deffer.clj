(ns cljs.deffer
  "Macros for deffing lots of protocols")

(defmacro def-binary-protocols
  [& args]
  `(do
     ~@(for [[prot-name meth-name] (partition 2 args)]
         `(defprotocol ~prot-name
            (~meth-name [~'this ~'other])))))