;;; +keys.el -*- lexical-binding: t; -*-

(map!
 (:when (modulep! :completion vertico)
   "C-s" #'consult-line))
