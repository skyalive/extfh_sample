       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-FILE ASSIGN TO "testfile.isam"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TEST-KEY
               FILE STATUS IS WS-STATUS.

           SELECT SEQ-FILE ASSIGN TO "seqfile.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEQ-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TEST-FILE.
       01  TEST-RECORD.
           05  TEST-KEY       PIC 9(05).
           05  TEST-DATA      PIC X(20).

       FD  SEQ-FILE.
       01  SEQ-RECORD.
           05  SEQ-DATA       PIC X(30).

       WORKING-STORAGE SECTION.
       01  WS-STATUS          PIC 9(02).
       01  WS-SEQ-STATUS      PIC 9(02).
       01  WS-DISPLAY-MSG     PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Starting EXTFH Sample App".

           PERFORM TEST-INDEXED-FILE.
           PERFORM TEST-SEQUENTIAL-FILE.

           DISPLAY "Sample App Completed".
           STOP RUN.

       TEST-INDEXED-FILE.
           DISPLAY "--- Testing INDEXED File ---".
           OPEN OUTPUT TEST-FILE.
           IF WS-STATUS NOT = "00"
               DISPLAY "Error opening file: " WS-STATUS
               GO TO END-INDEXED
           END-IF.

           MOVE 12345 TO TEST-KEY.
           MOVE "Hello World" TO TEST-DATA.
           WRITE TEST-RECORD.
           IF WS-STATUS = "00"
               DISPLAY "Record written successfully"
           ELSE
               DISPLAY "Error writing record: " WS-STATUS
           END-IF.

           CLOSE TEST-FILE.

           OPEN INPUT TEST-FILE.
           MOVE 12345 TO TEST-KEY.
           READ TEST-FILE RECORD KEY IS TEST-KEY.
           IF WS-STATUS = "00"
               DISPLAY "Read Record: Key=" TEST-KEY " Data=" TEST-DATA
           ELSE
               DISPLAY "Error reading record: " WS-STATUS
           END-IF.
           CLOSE TEST-FILE.
       END-INDEXED.
           EXIT.

       TEST-SEQUENTIAL-FILE.
           DISPLAY "--- Testing SEQUENTIAL File ---".
           OPEN OUTPUT SEQ-FILE.
           MOVE "Sequential Line 1" TO SEQ-DATA.
           WRITE SEQ-RECORD.
           MOVE "Sequential Line 2" TO SEQ-DATA.
           WRITE SEQ-RECORD.
           CLOSE SEQ-FILE.

           OPEN INPUT SEQ-FILE.
           READ SEQ-FILE NEXT.
           DISPLAY "Read Seq: " SEQ-DATA.
           READ SEQ-FILE NEXT.
           DISPLAY "Read Seq: " SEQ-DATA.
           CLOSE SEQ-FILE.
           EXIT.
