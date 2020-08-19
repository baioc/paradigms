00	REM Fizz Buzz in BASIC
10	PRINT "Count until (type a number)"
15	INPUT N
20	LET C = 1
25	LET S = ""
30	IF C > N THEN 100
40	IF C % 3 <> 0 THEN 50
45	LET S = S + "Fizz"
50	IF C % 5 <> 0 THEN 70
55	LET S = S + "Buzz"
60	GOTO 80
70	IF C % 3 = 0 THEN 80
75	LET S = S + C
80	LET S = S + ", "
85	LET C = C + 1
90	GOTO 30
100	PRINT S + "done!"
