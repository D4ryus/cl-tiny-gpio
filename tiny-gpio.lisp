;;;; cl-tiny-gpio.lisp

(in-package #:tiny-gpio)

;; #define GPSET0 7
(defconstant +gp-set-0+ 7)
;; #define GPSET1 8
(defconstant +gp-set-1+ 8)

;; #define GPCLR0 10
(defconstant +gp-clr-0+ 10)
;; #define GPCLR1 11
(defconstant +gp-clr-1+ 11)

;; #define GPLEV0 13
(defconstant +gp-lev-0+ 13)
;; #define GPLEV1 14
(defconstant +gp-lev-1+ 14)

;; #define GPPUD     37
(defconstant +gp-pud+ 37)
;; #define GPPUDCLK0 38
(defconstant +gp-pud-clk-0+ 38)
;; #define GPPUDCLK1 39
(defconstant +gp-pud-clk-1+ 39)

;; #define PI_BANK (gpio>>5)
(defun pin-bank (pin)
  (ash pin -5))

;; #define PI_BIT  (1<<(gpio&0x1F))
(defun pin-bit (pin)
  (ash 1 (logand pin #x1f)))

;; Pin modes
;; #define PI_INPUT  0
;; #define PI_OUTPUT 1
;; #define PI_ALT0   4
;; #define PI_ALT1   5
;; #define PI_ALT2   6
;; #define PI_ALT3   7
;; #define PI_ALT4   3
;; #define PI_ALT5   2
(defvar +pin-modes+
  '((:in    . 0)
    (:out   . 1)
    (:alt-0 . 4)
    (:alt-1 . 5)
    (:alt-2 . 6)
    (:alt-3 . 7)
    (:alt-4 . 3)
    (:alt-5 . 2)))

;; void gpioSetMode(unsigned gpio, unsigned mode)
;; {
;;    int reg, shift;
;;    reg   =  gpio/10;
;;    shift = (gpio%10) * 3;
;;    gpioReg[reg] = (gpioReg[reg] & ~(7<<shift)) | (mode<<shift);
;; }
(defun pin-set-mode (mem pin mode)
  (let ((reg (floor pin 10))
        (shift (* 3 (mod pin 10))))
    (setf (cffi:mem-aref mem :uint32 reg)
          (logior (logand (cffi:mem-aref mem :uint32 reg)
                          (lognot (ash 7 shift)))
                  (ash (let ((mode-pair (assoc mode +pin-modes+)))
                         (unless mode-pair
                           (error "Unknown mode ~a, use one of: ~a"
                                  mode
                                  (mapcar #'car +pin-modes+)))
                         (cdr mode-pair))
                       shift)))))

;; int gpioGetMode(unsigned gpio)
;; {
;;    int reg, shift;
;;    reg   =  gpio/10;
;;    shift = (gpio%10) * 3;
;;    return (*(gpioReg + reg) >> shift) & 7;
;; }
(defun pin-get-mode (mem pin)
  (let ((reg (floor pin 10))
        (shift (* 3 (mod pin 10))))
    (car
     (find (logand #x7
                   (ash (cffi:mem-aref mem :uint32 reg)
                        (- shift)))
           +pin-modes+
           :key #'cdr))))


;; Pull up down modes
;; #define PI_PUD_OFF  0
;; #define PI_PUD_DOWN 1
;; #define PI_PUD_UP   2
;; void gpioSetPullUpDown(unsigned gpio, unsigned pud)
;; {
;;    *(gpioReg + GPPUD) = pud;
;;    usleep(20);
;;    *(gpioReg + GPPUDCLK0 + PI_BANK) = PI_BIT;
;;    usleep(20);
;;    *(gpioReg + GPPUD) = 0;
;;    *(gpioReg + GPPUDCLK0 + PI_BANK) = 0;
;; }
(defun pin-set-pull-up-down (mem pin pull-up-down)
  (setf (cffi:mem-aref mem :uint32 +gp-pud+)
        (ecase pull-up-down
          (:off 0)
          (:down 1)
          (:up 2)))
  (osicat-posix:usleep 20)
  (setf (cffi:mem-aref mem :uint32 (+ +gp-pud-clk-0+ (pin-bank pin)))
        (pin-bit pin))
  (osicat-posix:usleep 20)
  (setf (cffi:mem-aref mem :uint32 +gp-pud+) 0
        (cffi:mem-aref mem :uint32 (+ +gp-pud-clk-0+ (pin-bank pin))) 0))

;; int gpioRead(unsigned gpio)
;; {
;;    if ((*(gpioReg + GPLEV0 + PI_BANK) & PI_BIT) != 0) return 1;
;;    else                                         return 0;
;; }
(defun pin-read (mem pin)
  (not
   (eql 0
        (logand (pin-bit pin)
                (cffi:mem-aref mem :uint32 (+ +gp-lev-0+ (pin-bank pin)))))))

;; void gpioWrite(unsigned gpio, unsigned level)
;; {
;;    if (level == 0) *(gpioReg + GPCLR0 + PI_BANK) = PI_BIT;
;;    else            *(gpioReg + GPSET0 + PI_BANK) = PI_BIT;
;; }
(defun pin-write (mem pin level)
  (setf (cffi:mem-aref mem :uint32
                       (+ (pin-bank pin)
                          (if level
                              +gp-set-0+
                              +gp-clr-0+)))
        (pin-bit pin)))

;; void gpioTrigger(unsigned gpio, unsigned pulseLen, unsigned level)
;; {
;;    if (level == 0) *(gpioReg + GPCLR0 + PI_BANK) = PI_BIT;
;;    else            *(gpioReg + GPSET0 + PI_BANK) = PI_BIT;
;;    usleep(pulseLen);
;;    if (level != 0) *(gpioReg + GPCLR0 + PI_BANK) = PI_BIT;
;;    else            *(gpioReg + GPSET0 + PI_BANK) = PI_BIT;
;; }
(defun pin-trigger (mem pin pulse-len level)
  (setf (cffi:mem-aref mem :uint32
                       (+ (pin-bank pin)
                          (if (eql level 0)
                              +gp-clr-0+
                              +gp-set-0+)))
        (pin-bit pin))
  (osicat-posix:usleep pulse-len)
  (setf (cffi:mem-aref mem :uint32
                       (+ (pin-bank pin)
                          (if (not (eql level 0))
                              +gp-clr-0+
                              +gp-set-0+)))
        (pin-bit pin)))

;; uint32_t gpioReadBank1(void) { return (*(gpioReg + GPLEV0)); }
(defun read-bank-1 (mem)
  (cffi:mem-aref mem :uint32 +gp-lev-0+))

;; uint32_t gpioReadBank2(void) { return (*(gpioReg + GPLEV1)); }
(defun read-bank-2 (mem)
  (cffi:mem-aref mem :uint32 +gp-lev-1+))

;; void gpioClearBank1(uint32_t bits) { *(gpioReg + GPCLR0) = bits; }
(defun clear-bank-1 (mem bits)
  (setf (cffi:mem-aref mem :uint32 +gp-clr-0+) bits))

;; void gpioClearBank2(uint32_t bits) { *(gpioReg + GPCLR1) = bits; }
(defun clear-bank-2 (mem bits)
  (setf (cffi:mem-aref mem :uint32 +gp-clr-1+) bits))

;; void gpioSetBank1(uint32_t bits) { *(gpioReg + GPSET0) = bits; }
(defun set-bank-1 (mem bits)
  (setf (cffi:mem-aref mem :uint32 +gp-set-0+) bits))

;; void gpioSetBank2(uint32_t bits) { *(gpioReg + GPSET1) = bits; }
(defun set-bank-2 (mem bits)
  (setf (cffi:mem-aref mem :uint32 +gp-set-1+) bits))

(defmacro with-mem ((mem) &body body)
  (let ((fd (gensym "fd"))
        (size (gensym "size")))
    `(mmap:with-mmap (,mem ,fd ,size #p"/dev/gpiomem"
                           :open '(:write :file-sync)
                           :size #xb4
                           :protection '(:read :write)
                           :mmap '(:shared))
       ,@body)))
