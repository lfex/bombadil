;;;; The module provides wrapper functions for the Erlang TOML library as well
;;;; as providing support for converting parsed TOML as a map (instead of the
;;;; dicts used by the TOML library).
;;;;
;;;; As such, it makes extensive use of the Erlang TOML library:
;;;; * https://github.com/dozzie/toml
;;;; * http://dozzie.jarowit.net/api/erlang-toml/
(defmodule bombadil
  (export
   ;; TOML API wrappers
   (exists 2) (exists 3)
   (foldk 4)
   (folds 4)
   (format-error 1)
   (get-value 3) (get-value 4)
   (keys 2)
   (parse 1) (parse 2)
   (read-file 1) (read-file 2)
   (sections 2)
   ;; Bombadil API
   (read 1) (read 2)
   (section->map 2)
   (toml->map 1)
   ;; Utility functions
   (assoc 2)
   (assoc-in 3)
   (get-in 2)
   (get-path 2)
   (get-paths 1) (get-paths 3)
   (toml-> 1)))

;;; TOML API

(defun exists (section toml)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#exists-2"
  (toml:exists section toml))

(defun exists (section key toml)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#exists-3"
  (toml:exists section key toml))

(defun foldk (section func acc toml)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#foldk-4"
  (toml:foldk section func acc toml))

(defun folds (section func acc toml)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#folds-4"
  (toml:folds section func acc toml))

(defun format-error (reason)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#format_error-1"
  (toml:format_error reason))

(defun get-value (section key toml)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#get_value-3"
  (toml:get_value section key toml))

(defun get-value (section key toml default)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#get_value-4"
  (toml:get_value section key toml default))

(defun keys (section toml)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#keys-2"
  (toml:keys section toml))

(defun parse (string)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#parse-1"
  (toml:parse string))

(defun parse (string validator-func)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#parse-2"
  (toml:parse string validator-func))

(defun read-file (filename)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#read_file-1"
  (toml:read_file filename))

(defun read-file (filename validator-func)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#read_file-2"
  (toml:read_file filename validator-func))

(defun sections (section toml)
  "http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#sections-2"
  (toml:sections section toml))

;;; Bombadil API

(defun read (filename)
  (read filename '()))

(defun read (filename opts)
  (let ((toml (read-file filename))
        (return-map? (proplists:get_value 'return opts 'true)))
    (case toml
      (`#(ok ,result) (if return-map? (toml->map result) result))
      (err err))))

(defun section->map (toml section-path)
  (foldk section-path
         (lambda (_section k v acc)
           (maps:merge acc
                       `#m(,k ,(toml-> v))))
         #m()
         toml))

(defun toml->map (toml)
  (lists:foldl (match-lambda
                 (('() acc)
                  (maps:merge acc (section->map toml '())))
                 ((x acc)
                  (assoc-in acc x (section->map toml x))))
               #m()
               (get-paths toml)))

;;; Utility functions

;; Map helpers

(defun assoc
  ((map '())
   map)
  ((map `((,k ,v) . ,tail))
   (assoc (maps:merge map `#m(,k ,v)) tail)))

(defun assoc-in
  ((map '() _)
   map)
  ((map `(,k . ()) v)
   (assoc map `((,k ,v))))
  ((map `(,k . ,tail) v)
   (assoc map `((,k ,(assoc-in (maps:get k map #m()) tail v))))))

;; TOML Library Wrappers

(defun get-in
  ((toml keys) (when (andalso (is_tuple toml) (== (element 1 toml) 'dict)))
   (case (get-value (lists:droplast keys)
                    (lists:last keys)
                    toml)
     (`#(,_type ,v) v)
     ('none 'undefined)
     (result result)))
  ((data keys)
   (clj:get-in data keys)))

;;; Utility Functions

(defun get-path (toml path)
  (toml:folds path
              (lambda (toml s acc)
                (lists:append acc `(,s)))
              '()
              toml))

(defun get-paths (toml)
  (get-paths toml 'undefined '(())))

(defun get-paths
  ((_toml '() paths)
   paths)
  ((toml path paths)
   (let* ((path (if (is_list path) path '(())))
          (p (lists:append (lists:map (lambda (x)
                                        (get-path toml x))
                                      path)))
          (ps (++ paths p)))
     (get-paths toml p ps))))

(defun toml->
  ((`#(string ,string))
   string)
  ((`#(integer ,int))
   int)
  ((`#(float ,float))
   float)
  ((`#(boolean ,bool))
   bool)
  ((`#(datetime ,dt))
   dt)
  ((`#(array #(,_type ,list)))
   list)
  ((`#(empty ,list))
   list)
  ((`#(object #(,_type ,obj)))
   obj))