(load "interpreter.rkt")

; General testing framework 
(define test
  (lambda (testfile expected)
    (if (eq? (interpret testfile) expected)
        (string-append "Passed " testfile)
        (string-append "Failed " testfile))))

; Tests 1
(test "tests3/test1" 150)
(test "tests3/test2" -4)
(test "tests3/test3" 10)
(test "tests3/test4" 16)
(test "tests3/test5" 220)
(test "tests3/test6" 5)
(test "tests3/test7" 6)
(test "tests3/test8" 10)
(test "tests3/test9" 5)
(test "tests3/test10" -39)
(test "tests3/test11" 'true)
; Manually run test 12. Should give error 
(test "tests3/test13" 100)
(test "tests3/test14" 'false)
(test "tests3/test15" 'true)
(test "tests3/test16" 128)
; Manually run 17. Should give error 
(test "tests3/test18" 12)
(test "tests3/test19" 12)
(test "tests3/test20" 12)
