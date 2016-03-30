(load "interpreter.rkt")

; General testing framework 
(define test
  (lambda (testfile expected)
    (if (eq? (interpret testfile) expected)
        (string-append "Passed " testfile)
        (string-append "Failed " testfile))))

; Tests 1
(test "tests3/test1.txt" 10)
(test "tests3/test2.txt" 14)
(test "tests3/test3.txt" 45)
(test "tests3/test4.txt" 55)
(test "tests3/test5.txt" 1)
(test "tests3/test6.txt" 115)
(test "tests3/test7.txt" 'true)
(test "tests3/test8.txt" 20)
(test "tests3/test9.txt" 24)
(test "tests3/test10.txt" 2)
(test "tests3/test11.txt" 35)
; Manually run test 12. Should give error 
(test "tests3/test13.txt" 90)
(test "tests3/test14.txt" 69)
(test "tests3/test15.txt" 87)
(test "tests3/test16.txt" 64)
; Manually run 17. Should give error 
(test "tests3/test18.txt" 125)
(test "tests3/test19.txt" 100)
(test "tests3/test20.txt" 2000400)
