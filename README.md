# cl-tiny-gpio

XXX WIP

# Note

Requires newest version of mmap:

``` shell
cd ~/quicklisp/local-projects && git clone https://github.com/Shinmera/mmap
```

# Usage

``` common-lisp
;; write pin 13
(tiny-gpio:with-mem (mem)
  (tiny-gpio:pin-set-mode mem 13 :out)
  (tiny-gpio:pin-write mem 13 t))

;; read pin 5
(tiny-gpio:with-mem (mem)
  (tiny-gpio:pin-set-mode mem 5 :in)
  (tiny-gpio:pin-read mem 5))
```
