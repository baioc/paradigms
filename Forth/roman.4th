CREATE ROMANS      ( ones ) 'I' C, 'V' C,
                   ( tens ) 'X' C, 'L' C,
               ( hundreds ) 'C' C, 'D' C,
              ( thousands ) 'M' C,

VARIABLE COLUMN# ( mutable offset )

: ONES      0 COLUMN# ! ;
: TENS      2 COLUMN# ! ;
: HUNDREDS  4 COLUMN# ! ;
: THOUSANDS 6 COLUMN# ! ;

: COLUMN ( -- column address ) ROMANS COLUMN# @ + ;

: .SYMBOL ( offset -- )
	COLUMN +
	C@ EMIT ;

: .ONER  0 .SYMBOL ;
: .FIVER 1 .SYMBOL ;
: .TENER 2 .SYMBOL ;

: .ONERS ( #ones -- )
	?DUP IF 0 DO .ONER LOOP THEN ;

: .ALMOST ( quotient of 5 -- )
	.ONER
	IF .TENER ELSE .FIVER THEN ;

: .DIGIT ( digit -- )
	5 /MOD OVER
	4 = IF .ALMOST DROP
	ELSE IF .FIVER THEN .ONERS THEN ;


\ Emits the roman numeral equivalent of given number.
: .ROMAN ( number -- )
	1000 /MOD THOUSANDS .DIGIT
	 100 /MOD  HUNDREDS .DIGIT
	  10 /MOD      TENS .DIGIT
	               ONES .DIGIT ;
