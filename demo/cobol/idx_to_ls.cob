       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDX-TO-LS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDXFILE ASSIGN TO "IDXFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IDX-KEY
               FILE STATUS IS IDX-STATUS.
           SELECT OUTFILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  IDXFILE.
       01  IDX-REC.
           05 IDX-KEY               PIC X(10).
           05 IDX-DATA              PIC X(90).
       FD  OUTFILE.
       01  OUT-REC                  PIC X(100).

       WORKING-STORAGE SECTION.
       01  IDX-STATUS               PIC 9(02) VALUE 0.
       01  OUT-STATUS               PIC 9(02) VALUE 0.
       01  EOF-FLAG                 PIC X VALUE "N".
       01  READ-COUNT               PIC 9(4) VALUE 0.
       01  READ-LIMIT               PIC 9(4) VALUE 20.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "IDX-TO-LS START".
           OPEN INPUT IDXFILE
                OUTPUT OUTFILE.

           IF IDX-STATUS NOT = "00"
               DISPLAY "IDXFILE OPEN FAILED: " IDX-STATUS
               STOP RUN
           END-IF.

           IF OUT-STATUS NOT = "00"
               DISPLAY "OUTFILE OPEN FAILED: " OUT-STATUS
               STOP RUN
           END-IF.

           PERFORM UNTIL EOF-FLAG = "Y" OR READ-COUNT >= READ-LIMIT
               READ IDXFILE NEXT INTO OUT-REC
                   AT END
                       MOVE "Y" TO EOF-FLAG
               END-READ
               IF IDX-STATUS NOT = "00" AND IDX-STATUS NOT = "10"
                   DISPLAY "IDXFILE READ FAILED: " IDX-STATUS
                   MOVE "Y" TO EOF-FLAG
               END-IF
               IF EOF-FLAG = "N"
                   WRITE OUT-REC
                   ADD 1 TO READ-COUNT
               END-IF
           END-PERFORM.

           CLOSE IDXFILE OUTFILE.
           DISPLAY "IDX-TO-LS DONE".
           STOP RUN.
