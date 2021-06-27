# numget36
#### Extract numeric values from various string formats

> Converts Numeric Ascii Digits Found In A Character String Into a valid numberic value.

  Parameters:

  **Numeric-String**   X(36) Character Feild Containning The Numeric Digits Passed By The Caller to be processed.

  **Num36**     The processed ASCII Integer Value Of _Numeric-String_ Returned To The Caller.

  **Decptr**    16 bit Integer Returned To The Caller Representing The Number Of Decimal Places Found In The Number. 
  To be used as follows:
  ~~~ cobol
  Compute  NumFloat Rounded = (Num36 / ( 10 ** Decptr ))
  ~~~
  
  > Where _NumFloat_ should a computational with an implied decimal.

  **Editerror** 16 bit Integer A Non-Zero If _Numeric-String_ Did Not Contain Valid Numeric Data.

  **Nu-String** X(36) Character Feild Containning a new processed version of the Numeric Digits Passed By The Caller.
  
            If the Numeric-String contained a valid numeric value this field
            will contain a value that can be processed by the NUMVAL function.
