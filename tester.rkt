(load "interpreter.rkt")

; General testing framework 
(define test
  (lambda (testfile expected)
    (if (eq? (interpret testfile) expected)
        (string-append "Passed " testfile)
        (string-append "Failed " testfile))))

; Tests 1
(test "tests3/test1" 10)
(test "tests3/test2" 14)
(test "tests3/test3" 45)
(test "tests3/test4" 55)
(test "tests3/test5" 1)
(test "tests3/test6" 115)
(test "tests3/test7" 'true)
(test "tests3/test8" 20)
(test "tests3/test9" 24)
(test "tests3/test10" 2)
(test "tests3/test11" 35)
; Manually run test 12. Should give error 
(test "tests3/test13" 90)
(test "tests3/test14" 69)
(test "tests3/test15" 87)
(test "tests3/test16" 64)
; Manually run 17. Should give error 
(test "tests3/test18" 125)
(test "tests3/test19" 100)
(test "tests3/test20" 2000400)
