(load "interpreter.scm")

; General testing framework 
(define test
  (lambda (testfile class expected)
    (if (eq? (interpret testfile class) expected)
        (string-append "Passed " testfile)
        (string-append "Failed " testfile))))

; Tests 1
(test "tests4/test1.txt" 'A 15) ;A has main
(test "tests4/test2.txt" 'A 12) ;A has main
(test "tests4/test3.txt" 'A 125) ;A has main 
(test "tests4/test4.txt" 'A 36) ;A has main
(test "tests4/test5.txt" 'A 54) ;A has main
(test "tests4/test6.txt" 'A 110) ;A has main
(test "tests4/test7.txt" 'C 26) ;C has main
(test "tests4/test8.txt" 'Square 117) ;square has main
(test "tests4/test9.txt" 'Square 32) ;square has main
(test "tests4/test10.txt" 'List 15) ;List has main
(test "tests4/test11.txt" 'List 123456) ;List has main
(test "tests4/test12.txt" 'List 5285) ;List has main
(test "tests4/test13.txt" 'C -716) ;C has main