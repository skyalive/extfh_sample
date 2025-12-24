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

           MOVE 23456 TO TEST-KEY.
           MOVE "Second Record" TO TEST-DATA.
           WRITE TEST-RECORD.
           IF WS-STATUS = "00"
               DISPLAY "Second record written successfully"
           ELSE
               DISPLAY "Error writing second record: " WS-STATUS
           END-IF.

           MOVE 12345 TO TEST-KEY.
           MOVE "Dup Record" TO TEST-DATA.
           WRITE TEST-RECORD.
           IF WS-STATUS = "00"
               DISPLAY "Unexpected duplicate write success"
           ELSE
               DISPLAY "Duplicate write status: " WS-STATUS
           END-IF.

           MOVE "Hello Rewrite" TO TEST-DATA.
           REWRITE TEST-RECORD.
           IF WS-STATUS = "00"
               DISPLAY "Record rewritten successfully"
           ELSE
               DISPLAY "Error rewriting record: " WS-STATUS
           END-IF.

           CLOSE TEST-FILE.

           OPEN INPUT TEST-FILE.
           MOVE 12345 TO TEST-KEY.
           START TEST-FILE KEY >= TEST-KEY
               INVALID KEY
                   DISPLAY "Error starting read: " WS-STATUS
               NOT INVALID KEY
                   READ TEST-FILE NEXT
                   IF WS-STATUS = "00"
                       STRING
                           "Read Next: Key=" DELIMITED BY SIZE
                           TEST-KEY DELIMITED BY SIZE
                           " Data=" DELIMITED BY SIZE
                           TEST-DATA DELIMITED BY SIZE
                           INTO WS-DISPLAY-MSG
                       END-STRING
                       DISPLAY WS-DISPLAY-MSG
                   ELSE
                       DISPLAY "Error reading next: " WS-STATUS
                   END-IF
           END-START.
           READ TEST-FILE RECORD KEY IS TEST-KEY.
           IF WS-STATUS = "00"
               STRING
                   "Read Record: Key=" DELIMITED BY SIZE
                   TEST-KEY DELIMITED BY SIZE
                   " Data=" DELIMITED BY SIZE
                   TEST-DATA DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG
               END-STRING
               DISPLAY WS-DISPLAY-MSG
           ELSE
               DISPLAY "Error reading record: " WS-STATUS
           END-IF.
           DELETE TEST-FILE.
           IF WS-STATUS = "00"
               DISPLAY "Record deleted successfully"
           ELSE
               DISPLAY "Error deleting record: " WS-STATUS
           END-IF.
           READ TEST-FILE RECORD KEY IS TEST-KEY.
           IF WS-STATUS = "00"
               DISPLAY "Unexpected read after delete"
           ELSE
               DISPLAY "Read after delete status: " WS-STATUS
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
