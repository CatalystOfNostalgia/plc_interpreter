(load "interpreter.rkt")

; General testing framework 
(define test
  (lambda (testfile expected)
    (if (eq? (interpret testfile) expected)
        (string-append "Passed " testfile)
        (string-append "Failed " testfile))))

; Tests 1
(test "tests1/test1.txt" 150)
(test "tests1/test2.txt" -4)
(test "tests1/test3.txt" 10)
(test "tests1/test4.txt" 16)
(test "tests1/test5.txt" 220)
(test "tests1/test6.txt" 5)
(test "tests1/test7.txt" 6)
(test "tests1/test8.txt" 10)
(test "tests1/test9.txt" 5)
(test "tests1/test10.txt" -39)
; Manually run tests 11-14 before turning in 
(test "tests1/test15.txt" 'true)
(test "tests1/test16.txt" 100)
(test "tests1/test17.txt" 'false)
(test "tests1/test18.txt" 'true)
(test "tests1/test19.txt" 128)
(test "tests1/test20.txt" 12)

; Tests 2
(test "tests2/test1.txt" 20)
(test "tests2/test2.txt" 164)
(test "tests2/test3.txt" 32)
(test "tests2/test4.txt" 2)
; Manually run test 5
(test "tests2/test6.txt" 25)
(test "tests2/test7.txt" 21)
(test "tests2/test8.txt" 6)
(test "tests2/test9.txt" -1)
(test "tests2/test10.txt" 789)
; Manurally run tests 11-13 
(test "tests2/test14.txt" 12)
(test "tests2/test15.txt" 125)
(test "tests2/test16.txt" 110)
(test "tests2/test17.txt" 2000400)
(test "tests2/test18.txt" 101)
; Manuallly run test 19
