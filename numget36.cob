*> $title "NUMGET36 - GET A NUMERIC VALUE 9(36) FROM A X(36)"
*> $control Rlfile,list,dynamic,bounds,optfeatures=linkaligned
 Identification Division.
 Program-Id. Numget36.
 Author.     Michael Anderson.
 Date-Compiled.

*>************************************************************
*>
*>  Converts Numeric Ascii Digits Found In A Character String
*>  Into a Computational, Signed Number.
*>
*>  Parameters:
*>
*>  Numeric-String    X(36) Character Feild Containning The Numeric Digits
*>            Passed By The Caller to be processed.
*>
*>  Num36     The processed S9(36) ASCII Signed Integer Value Of
*>            Numeric-String Returned To The Caller.
*>
*>  Decptr    Integer Returned To The Caller Representing The
*>            Number Of Decimal Places Found In The Number.
*>            To be used as follows:
*>            Compute  NumFloat Rounded = (Num36 / ( 10 ** Decptr )).
*>            Where NumFloat should a computational with an implied decimal.
*>
*>  Editerror Integer Returns A Non-Zero If Numeric-String Did Not
*>            Contain Valid Numeric Data.
*>
*>  Nu-String X(36) Character Feild Containning a new processed version of
*>            the Numeric Digits Passed By The Caller.
*>            If the Numeric-String contained a valid numeric value this field
*>            will contain a value that can be processed by the NUMVAL function.
*>
*>************************************************************

 Environment Division.
 Configuration Section.
 Source-Computer. Hp3000.
 Object-Computer. Hp3000.
 Data Division.
 Working-Storage Section.
 01  Nu-String              Pic X(36) Value Spaces.
 01  Nu-Len                 Pic S9(4) Comp Value 0.
 01  My-Char                Pic X(36) Value Spaces.
 01  N                     Pic 9(4) Comp.
 01  PassedLength          Pic 9(4) Comp.
 01  Charptr               Pic 9(4) Comp.
 01  Digitptr              Pic 9(4) Comp.
 01  DigitCount            Pic 9(4) Comp.
 01  Right-DigitCount      Pic 9(4) Comp.
 01  Left-DigitCount       Pic 9(4) Comp.
 01  Left-Decimal-Value    Pic 9(4) Comp.
 01  Right-Decimal-Value   Pic 9(4) Comp.
 01  Right-Numeric-Pos     Pic 9(4) Comp.
 01  Left-Numeric-Pos      Pic 9(4) Comp.

 01  Decimal-Sw            Pic X.
 01  Comma-Sw              Pic X.
 01  Mynum.
     02 Mynumber              Pic 9(36).
     02 N1 Redefines Mynumber Pic X Occurs 36 Times.

 01  Xptr                   Pic S9(4) Comp Value 0.
 01  Yptr                   Pic S9(4) Comp Value 0.
 01  Zptr                   Pic S9(4) Comp Value 0.
 01  Numlen                 Pic S9(4) Comp Value 0.
 01  Left-Zero-Count        Pic S9(4) Comp Value 0.
 01  Right-Zero-Count       Pic S9(4) Comp Value 0.
 01  Decimal-Places         Pic S9(4) Comp Value 0.
 01  NumFloat               Pic S9(18)V9(18) Comp-3 Value 0.
 01  ZoneFloat              Pic ------------------.------------------.
 01  StrFloat               Pic X(36) Value Spaces.
 01  DeciScale              Pic S9(4) Comp Value 0.
 01  DigitCnt               Pic S9(4) Comp Value 0.
*>
*> HP Overpunch Characters for Rightmost Digit in ASCII Numbers.
*> Digits Internal Representation denotes
*>    "Positive" "Negative"  or "No Sign", no sign is assumed positive.

 01 CNCTIDX Pic S9(4) Comp Value 0.

 01 POSINUMS-Conversion-Table.
   02 CNCT-POSINUMS Pic X(10) Value "{ABCDEFGHI".
   02 CNCT-POSITABLE Redefines CNCT-POSINUMS.
       05 CNCT-POSITABLE-Entries  Occurs 10 Times .
           10 CNCT-POSINUM Pic X.

 01 NEGANUMS-Conversion-Table.
    02 CNCT-NEGANUMS Pic X(10) Value "}JKLMNOPQR".
    02 CNCT-NEGATABLE Redefines CNCT-NEGANUMS.
       05 CNCT-NEGATABLE-Entries  Occurs 10 Times.
          10 CNCT-NEGANUM Pic X.

 01 CobNum-Conversion-Table.
    02 CNCT-NUMS     Pic X(10) Value "0123456789".
    02 CNCT-TABLE    Redefines CNCT-NUMS.
       05 CNCT-TABLE-Entries Occurs 10 Times.
          10 CNCT-NUM     Pic X.

 01 Punch-Char Pic X Value " ".
     88 OverPunch-NOSign   Values Are "0" "1" "2" "3" "4" "5" "6" "7" "8" "9".
     88 OverPunch-Positive Values Are "{" "A" "B" "C" "D" "E" "F" "G" "H" "I".
     88 OverPunch-Negative Values Are "}" "J" "K" "L" "M" "N" "O" "P" "Q" "R".

 01 NumError-Sw           Pic 9 Value 0.
    88 NumError           Value 1, False 0.

 01 Negative-Sign-Sw      Pic 9 Value 0.
    88 Negative-Sign      Value 1, False 0.

 01 In-Range-Sw           Pic 9 Value 0.
    88 In-Range           Value 1, False 0.

 01 Punch-Positive-Sw     Pic 9 Value 0.
    88 Punch-Positive     Value 1, False 0.

 01 Punch-Negative-Sw     Pic 9 Value 0.
    88 Punch-Negative     Value 1, False 0.

 01 Decimal-Value-Sw          Pic 9 Value 0.
    88 Decimal-Value          Value 1, False 0.

 01 Found-Left-Numeric-Sw     Pic 9 Value 0.
    88 Found-Left-Numeric     Value 1, False 0.

 01 Found-Left-PlusSign-Sw    Pic 9 Value 0.
    88 Found-Left-PlusSign    Value 1, False 0.

 01 Found-Left-MinusSign-Sw   Pic 9 Value 0.
    88 Found-Left-MinusSign   Value 1, False 0.

 01 Found-Left-Decimal-Sw     Pic 9 Value 0.
    88 Found-Left-Decimal     Value 1, False 0.

 01 Found-Right-Numeric-Sw    Pic 9 Value 0.
    88 Found-Right-Numeric    Value 1, False 0.

 01 Found-Right-PlusSign-Sw   Pic 9 Value 0.
    88 Found-Right-PlusSign   Value 1, False 0.

 01 Found-Right-MinusSign-Sw  Pic 9 Value 0.
    88 Found-Right-MinusSign  Value 1, False 0.

 01 Found-Right-Decimal-Sw    Pic 9 Value 0.
    88 Found-Right-Decimal    Value 1, False 0.

 Linkage Section.

 01  Numeric-String.
     02  X1                Pic X  Occurs 36 Times.
 01  Num36                 Pic S9(36).
 01  Decptr                Pic 9(4) Comp.
 01  Editerror             Pic S9(4) Comp.
 01  Linked-Nu-String      Pic X(36).

 Procedure Division Using Numeric-String Num36 Decptr Editerror Linked-Nu-String.
*>-----------------------------------------------------------------
 Begin-0000.
*>
*> NOTE: This routine uses a double parse method,
*>       first parsing from the LEFT side of the string,
*>       then parsing from the RIGHT.
*>
*>       Data collected in the LEFT parse is used in the RIGHT parse.
*>
*> Supports 17 digits on both sides of the decimal, and the sign, 36 bytes.
*> Supports the following characters:
*>    Comma, Minus, Plus, Decimal, and a dollar sign.
*>    Also the HP OverPunched characters.
*>
*> Input is limited to 36 bytes, Example:
*> Passing this: '$99,999,999,999,999,999.99999999999999999'
*> The receiving program only receives the following, without reporting an error.
*>               '$99,999,999,999,999,999.999999999999' (17,12)
*> So it,Returns this  '99999999999999999.999999999999' (17,12)
*>
*> Passing this: '-99999999999999999.99999999999999999' (17,17)
*> Returns this  '-99999999999999999.99999999999999999' (17,17)
*>
*> Passing this: '999999999999999999.999999999999999999' (18,18)
*> Returns this   '99999999999999999.99999999999999999'  (17,17)
*>
     Perform Initialization.
     IF Editerror < 0
        Go To Normal-End.

     Perform Left-Parse.
     IF Editerror < 0
        Go To Normal-End.

     Perform Right-Parse.
     IF Editerror < 0
        Go To Normal-End.

     Perform Process-Data.

     Go To Normal-End.

*>-----------------------------------------------------------------
 Initialization.
     Move Upper-Case(Trim(Numeric-String)) To Numeric-String.
     Move 0 To DigitCount Right-DigitCount Left-DigitCount Xptr N.
     Move 0 To Num36 Decptr Editerror.
     Move Length(Trim(Numeric-String,TRAILING)) To PassedLength.
     If PassedLength < 1
        *> Caller needs to pass us something that we can work with!
        Set NumError To True
        Move -1 To Editerror.

     Set NumError              To False.
     Set Negative-Sign         To False.
     Set Punch-Positive        To False.
     Set Punch-Negative        To False.
     Set Decimal-Value         To False.
     Set Found-Left-Numeric    To False.
     Set Found-Left-PlusSign   To False.
     Set Found-Left-MinusSign  To False.
     Set Found-Left-Decimal    To False.
     Set Found-Right-Numeric   To False.
     Set Found-Right-PlusSign  To False.
     Set Found-Right-MinusSign To False.
     Set Found-Right-Decimal   To False.

*>---------------------------------------------------------------
 Left-Parse.
     *>
     *> From the LEFT
     *> (Parse Numeric String From the Left)
     Perform Varying N From 1 By 1 Until NumError Or N > PassedLength
        If X1(N) > " "
           If X1(N) = "$"
              Move Space To X1(N)
           End-If

           If X1(N) = "0"
              Add 1 To Left-Zero-Count
              If Not Found-Left-Numeric
                 If Not Found-Left-Decimal
                    Move " " To X1(N)
                 End-If
              End-If
           End-If

           If X1(N) = "+"
              If Not Found-Left-Numeric
                 If Not Found-Left-PlusSign
                    Set Found-Left-PlusSign To True
                    Move Space To X1(N)
                 Else
                    *> More than one plus sign (+)
                    Set NumError To True
                    Move -2 To Editerror
                 End-If
              Else
                 *> Plus sign (+), is in the wrong position.
                 Set NumError To True
                 Move -3 To Editerror
              End-If
           End-If

           If X1(N) = "-"
              If Not Found-Left-Numeric
                 If Not Found-Left-MinusSign
                    Set Found-Left-MinusSign To True
                    Move Space To X1(N)
                 Else
                    *> More than one Minus sign (-)
                    Set NumError To True
                    Move -4 To Editerror
                 End-If
              Else
                 *> Minus sign (-), is in the wrong position.
                 Set NumError To True
                 Move -5 To Editerror
              End-If
           End-If

           If X1(N) = "."
              If Not Found-Left-Decimal
                 Set Found-Left-Decimal To True
                 Move N To Left-Decimal-Value
              Else
                 *> More than one decimal (dot) found in string
                 Set NumError To True
                 Move -6 To Editerror
              End-If
              Set Decimal-Value To True
           End-If

           If X1(N) Is Numeric
              Add 1 To Left-DigitCount
              If Not Found-Left-Numeric
                 Set Found-Left-Numeric To True
                 Move N To Left-Numeric-Pos
              End-If
           End-If

     End-Perform.
*>---------------------------------------------------------------
 Right-Parse.
     *>
     *> From the RIGHT
     *> (Parse Numeric String From the Right)
     Move 0 To Decimal-Places.
     Perform Varying N From PassedLength By -1 Until NumError Or N < 1

        If X1(N) Not = " "
           If N = PassedLength
              Move X1(N) To Punch-Char
              If ( OverPunch-Positive Or OverPunch-Negative )
                 Perform OverPunch-Conversion
              End-If
           End-IF

           If X1(N) = "0"
              Add 1 To Right-Zero-Count
              If Found-Left-Decimal and N <> PassedLength
                 If Not Found-Right-Numeric
                    If Not Found-Right-Decimal
                          Move " " To X1(N)
                    End-If
                 End-If
              End-If
           End-If

           If X1(N) = "."
              If Not Found-Right-Decimal
                 Set Found-Right-Decimal To True
                 Move N To Right-Decimal-Value
              Else
                 *> More than one decimal (dot) found in string
                 Set NumError To True
                 Move -7 To Editerror
              End-If
              Set Decimal-Value To True
           End-If

           If X1(N) = "+"
              If Not Found-Right-Numeric
                 If Not Found-Right-PlusSign
                    Set Found-Right-PlusSign To True
                    Move Space To X1(N)
                 Else
                    *> More than one plus sign (+)
                    Set NumError To True
                    Move -8 To Editerror
              Else
                 *> Emmbedded plus sign (+), should be only ascii digits
                 Set NumError To True
                 Move -9 To Editerror
              End-If
           End-If

           If X1(N) = "-"
              If Not Found-Right-Numeric
                 If Not Found-Right-MinusSign
                    Set Found-Right-MinusSign To True
                    Move Space To X1(N)
                 Else
                    *> More than one Minus sign (-)
                    Set NumError To True
                    Move -10 To Editerror
              Else
                 *> Emmbedded Minus sign (-), should be only ascii digits
                 Set NumError To True
                 Move -11 To Editerror
              End-If
           End-If

           If X1(N) Is Numeric
              Add 1 To Right-DigitCount
              If Not Found-Right-Numeric
                 Set Found-Right-Numeric To True
                 Move N To Right-Numeric-Pos
              End-If
              If Not Found-Right-Decimal
                 Add 1 To Decimal-Places
              End-If
           End-If
        End-If
     End-Perform.

*>---------------------------------------------------------------
 Process-Data.
     Compute DigitCount = ((Left-DigitCount + Right-DigitCount) / 2).

     *> This is the only use of DigitCount.
     IF NumError or DigitCount Not > 0
        If (Left-Zero-Count + Right-Zero-Count) Not > 0
           *> Invalid or Missing ASCII Digits.
           Move -13 To Editerror.

     If Found-Left-MinusSign or Found-Right-MinusSign
        Set Negative-Sign To True.

     *> Move Only The Numeric bytes to the New Variable
     Compute Numlen = ((Right-Numeric-Pos + 1) - Left-Numeric-Pos).
     If Numlen > 1
        Move Numeric-String(Left-Numeric-Pos:Numlen) To My-Char.

     Initialize Mynumber. *> NOTE: Mynumber a 36 digit Signed Zoned Numeric.
                          *>       redefined as N1, an array of 36 single bytes.

     *> Convert To Signed Integer Value
     Move 36 To Digitptr.
     Move Trim(Numeric-String) To Nu-String.
     Move Length(Trim(Nu-String)) To Nu-Len.
     Perform Varying N From Nu-Len By -1 Until N < 1 Or Digitptr < 1
        If Nu-String(N:1) >= "0" And Nu-String(N:1) <= "9"
           Move Nu-String(N:1) To N1(Digitptr)
           Compute Digitptr = Digitptr - 1 End-Compute
        Else
           If Nu-String(N:1) Not = "," and "." and "+" and "-" and " "
              *> Found some invalid characters
              *> Display "Setting error 14, because char=[" Nu-String(N:1) "]"
              Set NumError To True
              Move 14 To Editerror
           End-If
        End-If
     End-Perform.
*>   Move Numval(Mynumber) To Num36.
*>
*> IMPORTANT NOTE:
*>      In Version 2.4.3 of OpenCobol, from COBOL-IT;
*>      Numval does not work with 36 digits.
*>
     Move Mynumber To Num36. *> both are 36 bytes, and MOVE works fine without NUMVAL.

     *> Support negative values.
     If Negative-Sign or Punch-Negative
        Compute Num36 = ( Mynumber * -1).

     *> The real scaled value is:
     If Not Found-Right-Decimal
        Move 0 To Decimal-Places.

     If Decimal-Places < 1
        Move Num36 To NumFloat
     Else
        Compute  NumFloat Rounded = (Num36 / ( 10 ** Decimal-Places )).

     *> Now lets get on with it.
     Move NumFloat        To ZoneFloat.
     Move Trim(ZoneFloat) To StrFloat.

     Set In-Range To False.
     Perform Varying N From Length(StrFloat) by -1 Until In-Range Or N < 1

        If StrFloat(N:1) = "0"
           Move " " To StrFloat(N:1)
        End-If

        If StrFloat(N:1) = "."
           Move " " To StrFloat(N:1)
           Set In-Range To True
        End-If

        If StrFloat(N:1) is Numeric
           Set In-Range To True
        End-If

     End-Perform.
*>-----------------------------------------------------------------
 OverPunch-Conversion.
     Set Punch-Positive To False.
     Set Punch-Negative To False.
     If OverPunch-Positive
        Inspect Numeric-String converting CNCT-POSINUMS to CNCT-NUMS
        Set Punch-Positive To True.

     If OverPunch-Negative
        Inspect Numeric-String converting CNCT-NEGANUMS to CNCT-NUMS
        Set Punch-Negative To True.

     Move 16 To Editerror. *> Caller may want to know when this happens.
*>-----------------------------------------------------------------
 Normal-End.

     IF Negative-Sign and Punch-Negative
        *> Warn: Redundant signs, both are negative.
        Move 15 To Editerror.

     IF Found-Left-PlusSign or Found-Right-PlusSign
        If Punch-Negative
           *> ERROR: Redundant signs, One Positive and One Negative.
           Move -15 To Editerror.

     Move Trim(StrFloat) To Linked-Nu-String.
     Move Decimal-Places To Decptr.
     *>
     *> If passed 0, then a zero is returned in the New String.
     If Punch-Negative And Num36 = 0
        Move "-0" To Linked-Nu-String
        Move -0 To Num36.

     If Punch-Positive And Num36 = 0
        Move "0" To Linked-Nu-String.

     IF Editerror < 0
        Move Spaces To Linked-Nu-String.

*>
*> New edit on the num2 return value, and the decnum decimal places returned.
     Initialize DeciScale DigitCnt StrFloat.
     Move 36 To N.
     Perform Varying N From 36 By -1 Until N < 1

        *> Collect only numeric characters, and keep count,
        If Linked-Nu-String(n:1) <> " "
           If Linked-Nu-String(n:1) <> "."
              Add 1 To DigitCnt
              Move Linked-Nu-String(n:1) To StrFloat(DigitCnt:1)
           End-If
        End-If

        *> NOTE: Where is the decimal place.
        If Linked-Nu-String(n:1) = "."
           Move DigitCnt To DeciScale
        End-If

     End-Perform.
     Move Substitute(StrFloat,".", " ") To StrFloat.
     Move Reverse(StrFloat)            To StrFloat.
     Move Numval(Trim(StrFloat))       To Num36.
     Compute Decptr = DeciScale.
*>      Display "   After NUMGET, ASCII-Digits: " Numeric-String.
*>      Display "                        Num36: " Num36.
*>      Display "                       Decptr: " Decptr.
*>      Display "                    Editerror: " Editerror.
*>      Display "             Linked-Nu-String: " Linked-Nu-String.
*>      Display "     Numval(Linked-Nu-String): " Numval(Linked-Nu-String).

     Goback.

 End Program Numget36.
