;;; conf/+code.el -*- lexical-binding: t; -*-

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (python . t)
     (js . t)
     (cpp . t)
     )
   )
  )

(provide '+code)
