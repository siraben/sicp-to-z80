;;; compiler.scm --- compiling SICP register machine to TI-84 Z80 asm.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(define (member? a b)
  (cond ((null? b) #f)
        ((equal? a (car b)) #t)
        (else (member? a (cdr b)))))

(define (adjoin-set x set)
  (if (member? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((member? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	(else (union-set (cdr set1)
		         (adjoin-set (car set1) set2)))))


(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format #t "Not an output port ~s." p)))
     p)))

(define (emit . args)
  (apply format (compile-port) args)
  (format (compile-port) "")
  (newline (compile-port)))


(define (emit-no-newline . args)
  (apply format (compile-port) args)
  (format (compile-port) ""))


(define (make-generator prefix-string)
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (string->symbol
       (string-append prefix-string (number->string counter))))))

(define (tagged-list? x sym)
  (eq? (car x) sym))
(define (register? x)
  (tagged-list? x 'reg))
(define (label? x)
  (tagged-list? x 'label))
(define (register-name x)
  (and (register? x) (cadr x)))
(define (label-name x)
  (and (label? x) (cadr x)))


(define (compile-label exp)
  (emit "~a:" (sym-for-label (label-name exp))))


(define (extract-registers prog)
  (define (extract-acc prog acc)
    (cond
     ((null? prog) acc)
     ((eq? (caar prog) 'assign)
      (extract-acc
       (cdr prog)
       (adjoin-set (cadar prog) acc)))
     (else
      (extract-acc
       (cdr prog)
       acc))))
  (extract-acc prog '()))


(define (assign? x)
  (tagged-list? x 'assign))
(define (branch? x)
  (tagged-list? x 'branch))
(define (goto? x)
  (tagged-list? x 'goto))
(define (test? x)
  (tagged-list? x 'test))
(define (test-op x)
  (cadadr x))
(define (test-args x)
  (cddr x))
(define (op? x)
  (tagged-list? x 'op))
(define (op-arg x)
  (and (op? x) (cadr x)))
(define (const? x)
  (tagged-list? x 'const))
(define (const-name x)
  (and (const? x) (cadr x)))
(define (save? x)
  (tagged-list? x 'save))
(define (save-name x)
  (and (save? x) (cadr x)))
(define (restore? x)
  (tagged-list? x 'restore))
(define (restore-name x)
  (and (restore? x) (cadr x)))
(define (assign? x)
  (tagged-list? x 'assign))
(define (assign-reg-name x)
  (and (assign? x) (cadr x)))
(define (assign-args x)
  (and (assign? x) (cddr x)))

(define (increment? x)
  (tagged-list? x 'increment))
(define (decrement? x)
  (tagged-list? x 'decrement))

(define (compile-goto exp)
  (let ((arg (cadr exp)))
    (cond ((label? arg)
           (emit "        jp ~a" (sym-for-label (label-name arg))))
          ((and (register? arg)
                (eq? (register-name arg) 'continue))
           (emit "        ld hl, (~a)" (sym-for-reg 'continue))
           (emit "        jp (hl)")))))

(define (display? x)
  (tagged-list? x 'display))
(define (display-reg x)
  (and (display? x) (cadr x)))

(define (continue? x)
  (eq? x 'continue))

;; Assume (test (op <op>) <reg> <const>)

;; The SICP specification says (test (op <op>) <reg/const1> ...), but
;; we'll keep it simpler because Z80 assembly...

(define string-alist '())

(define string-generator (make-generator "String"))

(define (string-assq string alist)
  (if (null? alist)
      #f
      (if (string=? string (caar alist))
          (car alist)
          (string-assq string (cdr alist)))))

(define (symbol-for-string string)
  (let ((res (string-assq string string-alist)))
    (if res
        (cdr res)
        (let ((new-name (string-generator)))
          (set! string-alist
            `((,string . ,new-name) . ,string-alist))
          new-name))))

(define num-alist '())
(define num-generator (make-generator "Num"))

(define (symbol-for-num num)
  (let ((res (assq num num-alist)))
    (if res
        (cdr res)
        (let ((new-name (num-generator)))
          (set! num-alist
            `((,num . ,new-name) . ,num-alist))
          new-name))))

(define reg-alist '())
(define reg-generator (make-generator "Reg"))

(define (sym-for-reg reg)
  (let ((res (assq reg reg-alist)))
    (if res
        (cdr res)
        (let ((new-name (reg-generator)))
          (set! reg-alist
            `((,reg . ,new-name) . ,reg-alist))
          new-name))))

(define label-alist '())
(define label-generator (make-generator "Label"))

(define (sym-for-label label)
  (let ((res (assq label label-alist)))
    (if res
        (cdr res)
        (let ((new-name (label-generator)))
          (set! label-alist
            `((,label . ,new-name) . ,label-alist))
          new-name))))



(define (emit-string-declare string-sym-pair)
  (emit "~a: .db \"~a\",0"
        (cdr string-sym-pair)
        (car string-sym-pair)))

(define (emit-num-declare num-sym-pair)
  (emit "~a: .dw ~a"
        (cdr num-sym-pair)
        (car num-sym-pair)))

(define (compile-reg-or-const exp)
  (cond ((const? exp)
         (let ((var (const-name exp)))
           (cond ((number? var)
                  (emit-no-newline "~a" (symbol-for-num var)))
                 ;; Symbols and booleans aren't supported.
                 ((symbol? var)
                  (emit-no-newline "make_symbol(~a)" var))
                 ((boolean? var)
                  (emit-no-newline "make_boolean(~a)" (if var 1 0)))
                 ((string? var)
                  (emit-no-newline "~a" (symbol-for-string var))))))
        ((register? exp)
         (emit-no-newline "~a" (sym-for-reg exp)))))

(define (symbol-for-reg-or-const exp)
  (cond ((const? exp)
         (let ((var (const-name exp)))
           (cond ((number? var) (symbol-for-num var))
                 ;; Symbols and booleans aren't supported.
                 ((symbol? var)
                  (emit-no-newline "make_symbol(~a)" var))
                 ((boolean? var)
                  (emit-no-newline "make_boolean(~a)" (if var 1 0)))
                 ((string? var)
                  (symbol-for-string var)))))
        ((register? exp)
         (sym-for-reg (register-name exp)))))


;; We need this because we want to emit the correct branch (i.e. jp)
;; instruction later.
(define test-mode '())

(define (compile-test exp)
  (let ((compare-op (op-arg (cadr exp)))
        (num1       (symbol-for-reg-or-const (caddr exp)))
        (num2       (symbol-for-reg-or-const (cadddr exp))))
    
    (set! test-mode compare-op)
    
    (emit            "        ld ix, num1_low")
    (emit            "        ld hl, ~a" num1)
    (emit            "        inc hl")
    (emit            "        ld a, (hl)")
    (emit            "        ld (ix), a")
    
    (emit            "        ld ix, num2_low")
    (emit            "        ld hl, ~a" num2)
    (emit            "        inc hl")
    (emit            "        ld a, (hl)")
    (emit            "        ld (ix), a")

    (emit            "        ld ix, ~a" num1)
    (emit            "        ld a, (ix)")
    (emit            "        ld ix, ~a" num2)
    
    (emit            "        cp (ix)")))

(define (compile-branch exp)
  (if (label? (cadr exp))
      (let ((after-eq-label (sym-for-label (gensym "g"))))

        ;; We need to treat the equal test specially because
        ;; the low parts may not match.
        (if (eq? test-mode '=)
            (begin
              ;; We're testing for equality, save the place to
              ;; continue if it failed.
              (emit    "        ld hl, ~a" after-eq-label)
              (emit    "        push hl")))

        ;; The success label is now on top.
        (emit          "        ld hl, ~a" (sym-for-label (label-name (cadr exp))))
        (emit          "        push hl")

        ;; Only ONE of these instructions are emitted!
        (cond ((eq? test-mode '=) (emit          "        jp z, high_equals"))
              ((eq? test-mode '>) (emit          "        jp nc, high_great"))
              ((eq? test-mode '<) (emit          "        jp c, high_less"))
              (else
               (error "Branch was not preceded by a test.")))

        ;; If we failed to jump, we have to pop the continuation.
        (emit           "        pop hl")


        (if (eq? test-mode '=)
            (emit      "~a:"  after-eq-label))
        
        ;; We should only get here if the equal test failed because we
        ;; haven't compared the lower bits yet.
        
        ;; Clear the previous test.
        (set! test-mode '()))
      (error "The argument to branch is not a label.")))

(define (compile-save exp)
  (let ((register (sym-for-reg (save-name exp))))
    (emit "        ld hl, (~a)" register)
    (emit "        push hl")))

(define (compile-restore exp)
  (let ((register (sym-for-reg (restore-name exp))))
    (emit "        pop hl")
    (emit "        ld (~a), hl" register)))

(define (compile-assign exp)
  (let ((reg (sym-for-reg (assign-reg-name exp)))
        (args (assign-args exp)))
    (cond ((eq? (cadr exp) 'continue)
           (begin
             (emit "        ld ix, ~a" reg)
             (emit "        ld hl, ~a" (sym-for-label (label-name (car args))))
             (emit "        ld (ix), l")
             (emit "        ld (ix + 1), h")))
          ((= 3 (length args))
           (begin
             (emit-no-newline "~a = " reg)
             (let ((op (op-arg (car args)))
                   (op-args (cdr args)))
               (cond ((eq? '+ op)
                      (emit-no-newline "(**add_primop)"))
                     ((eq? '- op)
                      (emit-no-newline "(**sub_primop)"))
                     ((eq? '* op)
                      (emit-no-newline "(**mult_primop)")))
               (compile-op-args op-args 1))))
          ((= 1 (length args))
           (begin
             (emit            "        ld ix, ~a" reg)

             (emit-no-newline "        ld hl, (")
             (compile-reg-or-const (car args))
             (emit            ")")
             (emit            "        ld (ix), l")
             (emit            "        ld (ix + 1), h"))
           ))))

(define (compile-increment exp)
  (let ((reg (sym-for-reg (cadr exp))))
    (emit            "        ld hl, (~a)" reg)
    (emit            "        ld ix, ~a" reg)
    (emit            "        inc hl")
    (emit            "        ld (ix), l")
    (emit            "        ld (ix + 1), h")))

(define (compile-decrement exp)
  (let ((reg (sym-for-reg (cadr exp))))
    (emit            "        ld hl, (~a)" reg)
    (emit            "        ld ix, ~a" reg)
    (emit            "        dec hl")
    (emit            "        ld (ix), l")
    (emit            "        ld (ix + 1), h")))

(define reg-count 0)

(define (compile-declare-reg x)
  (set! reg-count (+ reg-count 1))
  (let ((offset (* reg-count 2)))
    (emit "~a .equ AppBackUpScreen+~a"
          (sym-for-reg x)
          offset)))

;; Something's wrong with this one...
(define fib-prog
  '((assign n (const 10))
    (assign continue (label fib_done))
    (assign val (const 0))

    (label fib_loop)
    (test (op <) (reg n) (const 2))
    (branch (label immediate_answer))
    (save continue)
    (assign continue (label afterfib_n_1))
    (save n) 
    (assign n (op -) (reg n) (const 1))
    (goto (label fib_loop))
    
    (label afterfib_n_1)
    (restore n)
    (restore continue)
    (assign n (op -) (reg n) (const 2))
    (save continue)
    (assign continue (label afterfib_n_2))
    (save val)
    (goto (label fib_loop))

    (label afterfib_n_2)
    (assign n (reg val))
    (restore val)
    (restore continue)
    (assign val (op +) (reg val) (reg n))
    (goto (reg continue))
    
    (label immediate_answer)
    (assign val (reg n))
    (goto (reg continue))
    (label fib_done)
    (display val)))

(define compile-expr
  (lambda (x)
    (cond ((test? x)
           (compile-test x))
          ((restore? x)
           (compile-restore x))
          ((branch? x)
           (compile-branch x))
          ((label? x)
           (compile-label x))
          ((goto? x)
           (compile-goto x))
          ((assign? x)
           (compile-assign x))
          ((save? x)
           (compile-save x))
          ((display? x)
           (compile-display x))
          ((increment? x)
           (compile-increment x))
          ((decrement? x)
           (compile-decrement x)))))

(define (compile-prog prog)
  (set! reg-count 0)
  (set! string-generator (make-generator "String"))
  (set! string-alist '())
  (set! num-generator (make-generator "Num"))
  (set! num-alist '())
  (set! reg-alist '())
  (set! reg-generator (make-generator "Reg"))
  
  (set! label-alist '())
  (set! label-generator (make-generator "Label"))

  
  
  (emit ".NOLIST")
  (emit "#include \"inc/ti83plus.inc\"")
  (emit ".LIST")
  (emit "cont .equ AppBackUpScreen")


  
  (emit "        .org 9D93h")
  (emit "        .db $BB,$6D")
  ;; Turn off run indicator.
  (emit "        b_call _RunIndicOff")
  ;; Clear the LCD.
  (emit "        b_call _ClrLCDFull")
  ;; Move the cursor to the top of the screen.
  (emit "        b_call _HomeUp")
  ;; Initialize memory.
  (emit "        call Init_ABS_mem")

  (for-each compile-expr prog)
  
  (emit "        b_call _GetKey")
  (emit "        ret")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helper routines
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Comparison routine.
  (emit "high_equals:")
  (emit "        ld ix, num2_low")
  (emit "        ld d, (ix)")
  (emit "        ld ix, num1_low")
  (emit "        ld a, (ix)")
  (emit "        cp d")

  ;; If we succeed we goto HL
  ;; If we fail to go to DE
  (emit "        pop hl")
  (emit "        pop de")
  (emit "        jp z, equal_succ")
  ;; We tried the comparison, didn't work.
  (emit "        ex  de,hl")
  (emit "        jp (hl)")
  ;; We don't need anything fancy here.
  (emit "high_less:")
  (emit "high_great:")
  (emit "        ret")
  (emit "equal_succ:")
  (emit "        jp (hl)")

  ;; Print a string without clobbering the registers.
  (emit "print_string_safe:")
  (emit "        push hl")
  (emit "        push af")
  (emit "        push de")
  (emit "        b_call _PutS")
  (emit "        b_call _Newline")
  (emit "        pop de")
  (emit "        pop af")
  (emit "        pop hl")
  (emit "        ret")

  ;; Print the contents of the HL register without clobbering the registers.
  (emit "printhl_safe:")
  (emit "        push hl")
  (emit "        push af")
  (emit "        push de")
  (emit "        b_call _DispHL")
  (emit "        b_call _Newline")
  (emit "        pop de")
  (emit "        pop af")
  (emit "        pop hl")
  (emit "        ret")

  ;; Clear the AppBackupScreen memory area
  (emit "Init_ABS_Mem:")
  (emit "        di")
  (emit "        ld    (save_sp), sp")
  (emit "        ld    sp, AppBackupScreen + 768")
  (emit "        ld    hl, $0000")
  (emit "        ld    b, 48")
  (emit "ABS_Clear_Loop:")
  (emit "        push  hl")
  (emit "        push  hl")
  (emit "        push  hl")
  (emit "        push  hl")
  (emit "        push  hl")
  (emit "        push  hl")
  (emit "        push  hl")
  (emit "        push  hl")
  (emit "        djnz  ABS_Clear_Loop")
  (emit "        ld    sp, (save_sp)")
  (emit "        ei")
  (emit "        ret")
  (emit "save_sp: .dw 0")
  (emit "num1_low:   .db 0")
  (emit "num2_low:   .db 0")

  (for-each emit-string-declare string-alist)
  (for-each emit-num-declare num-alist)
  (for-each compile-declare-reg (extract-registers prog))

  (emit "end"))

;; Need to compile operation expressions...
;; ((op ⟨operation-name⟩) ⟨input1⟩ . . . ⟨inputn⟩)
;; 


(define (compile-op-args exp headp)
  (cond ((null? exp)
         (emit ")"))
        ((eq? headp 1)
         (begin
           (emit-no-newline "(")
           (compile-reg-or-const (car exp))
           (compile-op-args (cdr exp) 0)))
        ((eq? headp 0)
         (begin
           (emit-no-newline ",")
           (compile-reg-or-const (car exp))
           (compile-op-args (cdr exp) 0)))))

(define simple-prog
  '((assign n (const 5))
    (display n)
    (save n)
    (assign n (op +) (reg n) (const 3))
    (display n)
    (restore n)
    (save n)))

;; The following program pushes the numbers 1 to 9 onto the stack and
;; restores them again.

(define counter-prog
  '((assign n (const 1))
    (label start_save)
    (display n)
    (test (op =) (reg n) (const 10))
    (branch (label done_save))
    (save n)
    (assign n (op +) (reg n) (const 1))
    (goto (label start_save))
    (label done_save)
    
    (label print_n)
    (display n)
    (restore n)
    (test (op =) (reg n) (const 1))
    (branch (label print_done))
    
    (goto (label print_n))
    (label print_done)))

;; Will go into an infinite loop printing 5 if the goto statement
;; wasn't saved successfully onto the stack.

(define goto-prog
  '((assign foo (const 5))
    (assign bar (const 1000))
    (label start)
    (display foo)
    (assign continue (label doge))
    (save continue)
    (assign continue (label start))
    (restore continue)
    (goto (reg continue))
    (label doge)
    (display bar)))

(define (compile-display x)
  (let ((obj (display-reg x)))
    ;; If the argument of the (display ...) instruction is a symbol,
    ;; then it must be a register.
    (if (symbol? obj)
        (begin
          (emit "        ld ix, ~a"   (sym-for-reg obj))
          (emit "        ld l, (ix)")
          (emit "        inc ix")
          (emit "        ld h, (ix)")
          (emit "        call printhl_safe"))
        ;; Otherwise, check if it's a string constant.
        (if (and (const? obj) (string? (cadr obj)))
            (begin
              (emit "        ld hl, ~a" (symbol-for-string (cadr obj)))
              (emit "        call print_string_safe"))))))

(define stack-prog
  '((assign n (const 1))
    (save n)
    (display n)

    ;; (assign n (op +) (reg n) (const 1))
    (assign n (const 2))
    (save n)
    (display n)
    
    ;; (assign n (op +) (reg n) (const 1))
    (assign n (const 3))
    (save n)
    (display n)

    (restore n)
    (display n)
    (restore n)
    (display n)
    (restore n)
    (display n)))



(define (compile-save exp)
  (let ((name (sym-for-reg (save-name exp))))
    (emit "        ld hl, (~a)" name))
  (emit "        push hl"))

(define (extract-registers prog)
  (define (extract-acc prog acc)
    (cond
     ((null? prog) acc)
     ((eq? (caar prog) 'assign)
      (extract-acc
       (cdr prog)
       (adjoin-set (cadar prog) acc)))
     (else
      (extract-acc
       (cdr prog)
       acc))))
  (extract-acc prog '()))


(define display-test
  '((assign dog (const 1234))
    (assign cat (const 5))
    (display dog)
    (display cat)))

(define z80-branch-test
  '((assign foo (const 432))
    (assign bar (const 432))
    
    (display (const "Value of foo"))
    (display foo)
    (display (const "Value of bar"))
    (display bar)

    (test (op =) (reg foo) (reg bar))
    (branch (label equal))
    (goto (label done))
    
    (label equal)
    (display (const "They are equal!"))

    (label done)
    (display (const "Done."))))

(define inc-dec-test
  '((assign foo (const 100))
    (display foo)
    (increment foo)
    (display foo)
    (decrement foo)
    (display foo)))

(define inc-add-test
  '((assign foo (const 100))
    (assign bar (const 0))
    (label inc_loop)
    (display bar)
    (increment bar)
    (increment bar)
    (decrement foo)
    (test (op =) (reg foo) (const 0))
    (branch (label done))
    (goto (label inc_loop))
    (label done)))


;; What is a subroutine?

;; A subroutine receives its arguments from the top of the stack, does
;; the computation then restores the registers and puts the result on
;; the top of the stack.

;; Call a procedure.
(define-syntax call
  (syntax-rules ()
    ((_ proc-name)
     (let ((after-proc (gensym "g")))
       `((assign continue (label ,after-proc))
         (goto (label proc-name))
         (label ,after-proc))))))

;; Save multiple registers.
(define-syntax save*
  (syntax-rules ()
    ((_ reg1 ...)
     `((save reg1)
       ...))))


;; Restore multiple registers.
;; This would allow you to do

;; ,@(save a b c)
;; ,@(restore a b c)

;; And registers a b and c are restored in the correct order.

(define-syntax restore*
  (syntax-rules ()
    ((_ reg ...)
     (map (lambda (x) `(restore ,x))
          (reverse '(reg ...))))))

(define-syntax print-string
  (syntax-rules ()
    ((_ msg)
     `((display (const msg))))))

;; Multi-assign
(define-syntax assign*
  (syntax-rules ()
    ((_ (reg1 val1) ...)
     `((assign reg1 (const val1))
       ...))))

(define-syntax define-program
  (syntax-rules (registers instructions)
    ((_ prog-name (registers (reg1 val1) ...) (instructions inst ...))
     (define prog-name
       `(,@(assign* (reg1 val1) ...)
         inst ...)))))

(define macro-insts
  '(define-procedure call print-string assign* restore* save*))


;; Define a procedure that can only be accessed by a call.
(define-syntax define-procedure
  (syntax-rules ()
    ((_ (name) inst ...)
     (let ((after_proc (gensym "g")))
       `((goto (label ,after_proc))
         (label name)
         inst ...
         (goto (reg continue))
         (label ,after_proc))))))


(define-program add-prog
  (registers (foo 0) (bar 0) (result 0) (aaa 0) (bbb 0) (ccc 0))
  
  (instructions

   (assign foo (const 1345))
   (display foo)
   (assign bar (const 1232))
   (display bar)
   
   (save foo)
   (save bar)

   
   ,@(call add)
   (restore result)
   (display result)
   
   ;; Generic add procedure; adds the top two elements on the stack.
   ;; The top element of the stack now becomes the result of the add
   ;; procedure.
   ,@(define-procedure (add)
       (restore aaa)
       (restore bbb)
       (assign result (reg bbb))
       
       (label add_loop)
       (decrement aaa)
       (increment bbb)
       (test (op >) (reg aaa) (const 1))
       (branch (label add_loop))
       (save result))))

;; Buggy, wtf?
(define subroutine-prog
  `((assign foo (const 3))
    (assign bar (const 5))
    (assign baz (const 0))


    (assign continue (label after-call))
    (save continue)
    ,@(call message1)
    (label after-call)
    ,@(print-string "Back to main.")
    (goto (label done))

    
    (label message1)
    ,@(print-string "White!")
    (goto (reg continue))

    (label message2)
    ,@(print-string "Gold!")
    (goto (reg continue))

    (label done)
    ,@(print-string "Done!")
    ))

(define test-prog
  `(,@(print-string "Hello.")
    ,@(print-string "This is a test")
    ,@(print-string "On the compiler that I wrote.")
    (assign foo (const 5))
    ,@(print-string "Foo:")
    (display foo)
    (increment foo)
    (display foo)
    (decrement foo)
    (display foo)
    ))

;; Works
(define counter-prog
  `((assign x (const 0))
    (label dec-loop)
    (increment x)
    (display x)
    (test (op >) (reg x) (const 10))
    (branch (label succ))
    (goto (label dec-loop))
    
    (label succ)
    (display (const "Succeeded!"))))

