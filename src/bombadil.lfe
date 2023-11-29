(defmodule bombadil
  (export
   (keys 1)
   (get 2) (get 3)
   (get-in 2) (get-in 3)
   (assoc 2)
   (assoc-in 3)
   (parse 1)
   (read 1)
   (format-error 1))
  ;; For the Erlangers:
  (export
   (get_in 2) (get_in 3)
   (assoc_in 3)
   (format_error 1)))

(defun keys (parsed)
  (maps:keys parsed))

(defun get
  ((parsed key) (when (is_binary key))
   (mref parsed key))
  ((parsed key)
   (get parsed (key->bin key))))

(defun get (parsed key default)
  (get parsed key))

(defun get-in
  ((parsed keys)
   (let ((keys (keys->bin keys)))
     (case (tomerl:get parsed keys)
       (`#(ok ,results) results)
       (err err)))))

(defun get-in (parsed key default)
  (get-in parsed key))

(defun assoc
  ((data '())
   data)
  ((data `((,k ,v) . ,tail))
   (assoc (maps:merge data `#m(,(key->bin k) ,v)) tail)))

(defun assoc-in
  ((data '() _)
   data)
  ((data `(,k . ()) v)
   (assoc data `((,k ,v))))
  ((data `(,k . ,tail) v)
   (assoc data `((,k ,(assoc-in (maps:get (key->bin k) data #m()) tail v))))))

(defun parse (data)
  (tomerl:parse data))

(defun read (filename)
  (tomerl:read_file filename))

(defun format-error (reason)
  (tomerl_parser:format_error reason))

;;; For the Erlangers

(defun get_in (p k) (get-in p k))
(defun get_in (p k d) (get-in p k d))
(defun assoc_in (d l v) (assoc-in d l v))
(defun format_error (r) (format-error r))

;;; Private functions

(defun key->bin (key)
  (list_to_binary (io_lib:format "~s" (list key))))

(defun keys->bin (keys)
  (list-comp ((<- k keys))
    (if (is_binary k)
      k
      (key->bin k))))
