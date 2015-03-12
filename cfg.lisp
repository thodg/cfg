;;
;;  cfg  -  shared project configuration
;;
;;  Copyright 2015 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :cl-user)

(defpackage :cfg
  (:use :cl)
  (:export
   #:*environment*
   #:*environments*
   #:cfg
   #:getenv))

(in-package :cfg)

(defun getenv (name &optional default)
  "UNIX getenv. See getenv(3)."
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defvar *environment* :development
  "Symbol naming the current configuration environment.")

(defvar *environments*
  (make-hash-table :test 'eq)
  "Hash table of configuration environments.")

(defun environment (&optional (name *environment*))
  "Retrieve or create an environment named NAME."
  (or #1=(gethash name *environments*)
      (setf #1# (make-hash-table :test 'eq))))

(defmacro cfg (key &optional default (environment ''*environment*))
  "Returns a place for configuration KEY value in ENVIRONMENT."
  `(gethash ,key (environment ,environment) ,default))

(defun configure (environment &rest plist)
  "Set multiple keys in ENVIRONMENT."
  (let ((*environment* environment))
    (loop while plist
          for key = (pop plist)
          for value = (pop plist)
       do (setf (cfg key) value))))
