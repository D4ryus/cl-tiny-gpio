;;;; cl-tiny-gpio.asd

(asdf:defsystem #:cl-tiny-gpio
  :description "Tiny gpio library in cl (see http://abyz.me.uk/rpi/pigpio)"
  :author "d4ryus <d4ryus@teknik.io>"
  :license  "Public domain"
  :version "0.0.1"
  :depends-on (#:cffi
               #:mmap
               #:osicat)
  :serial t
  :components ((:file "package")
               (:file "tiny-gpio")))
