;;;; package.lisp

(defpackage #:tiny-gpio
  (:use #:cl)
  (:export #:+gp-set-0+
           #:+gp-set-1+
           #:+gp-clr-0+
           #:+gp-clr-1+
           #:+gp-lev-0+
           #:+gp-lev-1+
           #:+gp-pud+
           #:+gp-pud-clk-0+
           #:+gp-pud-clk-1+
           #:pin-bank
           #:pin-bit
           #:+pin-modes+
           #:pin-set-mode
           #:pin-get-mode
           #:pin-set-pull-up-down
           #:pin-read
           #:pin-write
           #:pin-trigger
           #:read-bank-1
           #:read-bank-2
           #:clear-bank-1
           #:clear-bank-2
           #:set-bank-1
           #:set-bank-2
           #:with-mem))
