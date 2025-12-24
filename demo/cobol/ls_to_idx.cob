       IDENTIFICATION DIVISION.
       PROGRAM-ID. LS-TO-IDX.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS IN-STATUS.
           SELECT IDXFILE ASSIGN TO "IDXFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IDX-KEY
               FILE STATUS IS IDX-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  IN-REC                  PIC X(100).
       FD  IDXFILE.
       01  IDX-REC.
           05 IDX-KEY               PIC X(10).
           05 IDX-DATA              PIC X(90).

       WORKING-STORAGE SECTION.
       01  IN-STATUS                PIC 9(02) VALUE 0.
       01  IDX-STATUS               PIC 9(02) VALUE 0.
       01  EOF-FLAG                 PIC X VALUE "N".
       01  READ-COUNT               PIC 9(4) VALUE 0.
       01  READ-LIMIT               PIC 9(4) VALUE 20.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "LS-TO-IDX START".
           OPEN INPUT INFILE.

           IF IN-STATUS NOT = "00"
               DISPLAY "INFILE OPEN FAILED: " IN-STATUS
               STOP RUN
           END-IF.
           DISPLAY "INFILE OPEN OK".

           OPEN OUTPUT IDXFILE.

           IF IDX-STATUS NOT = "00"
               DISPLAY "IDXFILE OPEN FAILED: " IDX-STATUS
               STOP RUN
           END-IF.
           DISPLAY "IDXFILE OPEN OK".

           PERFORM UNTIL EOF-FLAG = "Y" OR READ-COUNT >= READ-LIMIT
               READ INFILE NEXT
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       MOVE IN-REC TO IDX-REC
                       WRITE IDX-REC
                       ADD 1 TO READ-COUNT
               END-READ
               IF IN-STATUS NOT = "00" AND IN-STATUS NOT = "10"
                   DISPLAY "INFILE READ FAILED: " IN-STATUS
                   MOVE "Y" TO EOF-FLAG
               END-IF
               IF READ-COUNT > 1000
                   DISPLAY "READ LIMIT REACHED"
                   MOVE "Y" TO EOF-FLAG
               END-IF
           END-PERFORM.

           CLOSE INFILE IDXFILE.
           DISPLAY "LS-TO-IDX DONE".
           STOP RUN.
