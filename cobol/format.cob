        >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. format.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
SELECT InputFile ASSIGN TO DYNAMIC filename
ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
    FD InputFile.
    01 inputLine PIC X(512).
WORKING-STORAGE SECTION.
    01 END-OF-FILE PIC Z(1).
    01 filename PIC X(255).
    01 currLine PIC X(60).
    01 currLineNum PIC 999999 VALUE 1.
    01 maxLine PIC X(60) VALUE " ".
    01 maxLineNum PIC 999999 VALUE 0.
    01 minLine PIC X(60) VALUE "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA".
    01 minLineNum PIC 999999 VALUE 0.
    01 formatted-int PIC ZZZZZZZ9.
    01 left-justified-int PIC X(13).
    01  tally-count pic 99 value zero.



PROCEDURE DIVISION.
    Begin.
        *>Read in filename from command line
        ACCEPT filename FROM COMMAND-LINE

        *>read in lines and store them all in bigString
        OPEN INPUT InputFile
        READ InputFile
            AT END MOVE 1 TO END-OF-FILE
        END-READ

        IF END-OF-FILE = 1
            CLOSE InputFile
        END-IF

        MOVE 0 TO END-OF-FILE.

        PERFORM UNTIL END-OF-FILE = 1
            CALL 'process-line' USING inputLine, currLine, currLineNum, maxLine, maxLineNum, minLine, minLineNum
            READ InputFile
                AT END MOVE 1 TO END-OF-FILE
            END-READ
        END-PERFORM
        CLOSE InputFile

        MOVE currLineNum TO formatted-int
        DISPLAY formatted-int SPACE SPACE currLine
        DISPLAY SPACE

        IF function length(function trim (currLine)) >= function length(function trim(maxLine)) THEN
            MOVE currLine TO maxLine
            MOVE currLineNum TO maxLineNum
        END-IF

        IF function length(function trim (currLine)) <= function length(function trim(minLine)) THEN
            MOVE currLine TO minLine
            MOVE currLineNum TO minLineNum
        END-IF


        inspect maxLineNum tallying tally-count for leading zeros.
        move maxLineNum (tally-count + 1 : length of maxLineNum - tally-count) to left-justified-int.
        DISPLAY "LONG   " left-justified-int maxLine

        MOVE ZERO TO tally-count
        inspect minLineNum tallying tally-count for leading zeros.
        move minLineNum (tally-count + 1 : length of minLineNum - tally-count) to left-justified-int.
        DISPLAY "SHORT  " left-justified-int minLine


    GOBACK
    .
END PROGRAM format.

IDENTIFICATION DIVISION.
PROGRAM-ID. remove-nums.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  I       PIC 999.
01  result  PIC X(100) VALUE SPACE.


LINKAGE SECTION.
01  Str     PIC X ANY LENGTH.

PROCEDURE DIVISION USING Str.
    PERFORM VARYING I FROM 1 BY 1
            UNTIL I > function length(function trim(Str))

            IF NOT (Str(I:1) = "0" OR
                    Str(I:1) = "1" OR
                    Str(I:1) = "2" OR
                    Str(I:1) = "3" OR
                    Str(I:1) = "4" OR
                    Str(I:1) = "5" OR
                    Str(I:1) = "6" OR
                    Str(I:1) = "7" OR
                    Str(I:1) = "8" OR
                    Str(I:1) = "9") THEN
                STRING
                    result DELIMITED BY SPACE
                    Str(I:1) DELIMITED BY SIZE
                    INTO result
                END-STRING
            END-IF

    END-PERFORM
    MOVE function trim(result) TO Str

    GOBACK
    .
END PROGRAM remove-nums.

IDENTIFICATION DIVISION.
PROGRAM-ID. process-line.

DATA DIVISION.
LOCAL-STORAGE SECTION.

01  I           PIC 999.
01  word        PIC X(100) VALUE SPACE.
01  line-length PIC 999.

LINKAGE SECTION.
01  Str     PIC X ANY LENGTH.
01  currLine PIC X(60).
01 currLineNum PIC 999999.
01 maxLine PIC X(60).
01 maxLineNum PIC 999999 VALUE 0.
01 minLine PIC X(60).
01 minLineNum PIC 999999 VALUE 0.


PROCEDURE DIVISION USING Str, currLine, currLineNum, maxLine, maxLineNum, minLine, minLineNum.

    MOVE function length (function trim(Str)) TO line-length *>calculate the trimmed line length
    PERFORM VARYING I FROM 1 BY 1
            UNTIL I > line-length*> loop util the end of string

            IF Str (I:1) = SPACE THEN
                CALL 'process-word' USING word, currLine, currLineNum, maxLine, maxLineNum, minLine, minLineNum
                *>DISPLAY  function trim(word)
                MOVE SPACE TO word

            ELSE
                STRING function trim(word) DELIMITED BY SIZE
                    Str(I:1) DELIMITED BY SIZE
                    INTO word
                END-STRING
            END-IF



    END-PERFORM

    CALL 'process-word' USING word, currLine, currLineNum, maxLine, maxLineNum, minLine, minLineNum
    *>DISPLAY  function trim(word)

    GOBACK
    .
END PROGRAM process-line.

IDENTIFICATION DIVISION.
PROGRAM-ID. process-word.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01 formatted-int PIC ZZZZZZZ9.


LINKAGE SECTION.
01  word     PIC X ANY LENGTH.
01  currLine PIC X(60).
01 currLineNum PIC 999999.
01 maxLine PIC X(60).
01 maxLineNum PIC 999999 VALUE 0.
01 minLine PIC X(60).
01 minLineNum PIC 999999 VALUE 0.


PROCEDURE DIVISION USING word, currLine, currLineNum, maxLine, maxLineNum, minLine, minLineNum.

    CALL 'remove-nums' USING word *>remove numbers from the word before processing it

    IF word = SPACE THEN
        GOBACK *>do nothing!
    END-IF

    IF currLine = SPACE THEN
      MOVE function trim(word) TO currLine
      GOBACK
    END-IF

    IF (60 >= function length (function trim(currLine)) + function length( function trim(word)) + 1) THEN
      STRING
          function trim(currLine) DELIMITED BY SIZE
          " " DELIMITED BY SIZE
          function trim(word) DELIMITED BY SIZE
          INTO currLine
      END-STRING
    ELSE
        MOVE currLineNum to formatted-int
        DISPLAY formatted-int SPACE SPACE currLine

        IF function length(function trim (currLine)) >= function length(function trim(maxLine)) THEN
            MOVE currLine TO maxLine
            MOVE currLineNum TO maxLineNum
        END-IF

        IF function length(function trim (currLine)) <= function length(function trim(minLine)) THEN
            MOVE currLine TO minLine
            MOVE currLineNum TO minLineNum
        END-IF

        ADD 1 TO currLineNum
        MOVE function trim(word) TO currLine
    END-IF

    GOBACK
    .
END PROGRAM process-word.
