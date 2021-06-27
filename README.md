# numget36
#### Extract numeric values from various string formats

> Converts Numeric Ascii Digits Found In A Character String Into a valid numberic value.
> ###### In COBOL _MOVE_ is really a copy with intelligent _Type_ conversion. If the conversion fails you will get run-time errors. If the conversion will be impossible, you will get a compile error, like trying to **MOVE** a string to an integer. 
> ###### If you want to represent a string as an integer you would use _REDEFINE_. Using _REDEFINE_ is how you might find **over-punch** characters in a numeric string, and why I've included the over-punch logic in this routine. Sometimes _Simplicity_ is complicated business.

  Parameters:
  ~~~cobol
   Linkage Section.
 01  Numeric-String.
     02  X1                Pic X  Occurs 36 Times.
 01  Num36                 Pic S9(36).
 01  Decptr                Pic 9(4) Comp.
 01  Editerror             Pic S9(4) Comp.
 01  Linked-Nu-String      Pic X(36).
 ~~~
 
  **Numeric-String**   X(36) Character Feild Containning The Numeric Digits Passed By The Caller to be processed.

  **Num36**     The processed ASCII Integer Value Of _Numeric-String_ Returned To The Caller.

  **Decptr**    16 bit Integer Returned To The Caller Representing The Number Of Decimal Places Found In The Number. 

  **Editerror** 16 bit Integer A Non-Zero If _Numeric-String_ Did Not Contain Valid Numeric Data.

  **Nu-String** X(36) Character Feild Containning a new processed version of the Numeric Digits Passed By The Caller.
  
   To be used as follows:
  ~~~ cobol
  Call "numget36" Using some-num-str, Num36, Decptr, Editerror, Linked-Nu-String
  If Editerror = 0 Then
     If Decptr > 0 Then
        Compute  NumFloat Rounded = (Num36 / ( 10 ** Decptr ))
      Else
        Move Num36 to NumFloat
      End-If
   Else
      Move 0 to NumFloat
      Perform Numerr-Processing
   End-IF
  ~~~
  
  > Where _NumFloat_ should be a computational with an implied decimal.
  > If the Numeric-String contained a valid numeric value this field will contain a value that may be processed by the NUMVAL function.
