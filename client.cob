       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROGRAM-CLIENT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RECEIVED-FILE ASSIGN TO RECEIVED-FILE-NAME
           ORGANIZATION IS SEQUENTIAL.
           SELECT FAILED-BLOCKS-FILE ASSIGN TO "tchurus.bin"
           ORGANIZATION IS SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.
           FD RECEIVED-FILE.
           01  FILE-PART.
               03  ACTUAL-FILE-PART BINARY-CHAR OCCURS 50.

           FD FAILED-BLOCKS-FILE.
           01 FAILED-BLOCK-NUM PIC 99999999.

       WORKING-STORAGE SECTION.
       
       01  RECEIVED-FILE-NAME PIC X(20).
      *socket creation
       01 SOCKET-NAMESPACE BINARY-LONG VALUE 2.
       01 SOCKET-STYLE BINARY-LONG VALUE 2.
       01 SOCKET-PROTOCOL BINARY-LONG VALUE 0.
       01 SOCKET-DESCRIPTOR BINARY-LONG.

      *socket option creation
       01  SOCKET-LEVEL BINARY-LONG VALUE 1.
       01  SOCKET-OPTION-LEVEL BINARY-LONG VALUE 1.
       01  SOCKET-OPTION-NAME BINARY-LONG VALUE 2.
       01  SOCKET-OPTION-VALUE-INT BINARY-LONG VALUE 1.
       01  SOCKET-OPTION-VALUE-TIMEVAL.
           03 SEC BINARY-DOUBLE VALUE 0.
           03 USEC BINARY-DOUBLE VALUE 0.

      *address of the socket in the server to send the message
       01 SERVER-SOCKET-ADDRESS.
            03  FAMILY BINARY-SHORT VALUE 2.
            03  PORT BINARY-SHORT VALUE 8080.
            03  IP BINARY-LONG VALUE 0.
            03  SIN-ZERO BINARY-CHAR OCCURS 8.
       01 SERVER-SOCKET-SIZE BINARY-LONG.

      *address of the socket that will listen to the response
       01 CLIENT-SOCKET-ADDRESS.
            03  FAMILY BINARY-SHORT VALUE 2.
            03  PORT BINARY-SHORT VALUE 8080.
            03  IP BINARY-LONG VALUE 0.
            03  SIN-ZERO BINARY-CHAR OCCURS 8.
       

       01  REQUEST-MSG PIC X(128).
       01  RECEIVED-MSG.
           03  BLOCK-INDEX BINARY-DOUBLE.
           03  TOTAL-BLOCKS BINARY-DOUBLE. 
           03  RECEIVED-MSG-DATA.
               05  ACTUAL-RECEIVED-MSG BINARY-CHAR OCCURS 50.
           03  CHECKSUM BINARY-DOUBLE.

       01  CALCULATED-CHECKSUM BINARY-DOUBLE.

       01  I BINARY-LONG.
       01  WAITED-BLOCK-INDEX BINARY-LONG.

       01  FAILED-BLOCK-INDEX BINARY-LONG.
       01  FAILED-BLOCK-INDEX-STR PIC 99999999.

       01  IS-EOF BINARY-SHORT VALUE 0.

       01  RECEIVE-SUCCESS BINARY-SHORT VALUE 0.
       
       PROCEDURE DIVISION.
           
           CALL 'socket' USING
               BY VALUE SOCKET-NAMESPACE
               BY VALUE SOCKET-STYLE
               BY VALUE SOCKET-PROTOCOL
               GIVING SOCKET-DESCRIPTOR
           END-CALL
           DISPLAY "SOCKET-DESCRIPTOR: " SOCKET-DESCRIPTOR
        
           
           PERFORM SET-SOCKET-REUSEADDR.
           PERFORM SET-SOCKET-TIMEOUT.

           CALL 'htons' USING
               BY VALUE 8081
               GIVING PORT OF CLIENT-SOCKET-ADDRESS
           END-CALL

           CALL "bind" USING 
               BY VALUE SOCKET-DESCRIPTOR
               BY REFERENCE CLIENT-SOCKET-ADDRESS
               BY VALUE LENGTH OF CLIENT-SOCKET-ADDRESS
           END-CALL
           DISPLAY "bind: " RETURN-CODE

           CALL 'htons' USING
               BY VALUE 8080
               GIVING PORT OF SERVER-SOCKET-ADDRESS
           END-CALL

           DISPLAY "TYPE FILENAME:"
           ACCEPT RECEIVED-FILE-NAME

           STRING "GET/" DELIMITED BY SIZE, 
           RECEIVED-FILE-NAME DELIMITED BY SIZE 
           INTO REQUEST-MSG. 

           DISPLAY REQUEST-MSG

           CALL "sendto" USING
               BY VALUE SOCKET-DESCRIPTOR
               BY REFERENCE REQUEST-MSG
               BY VALUE LENGTH OF REQUEST-MSG
               BY VALUE 0
               BY REFERENCE SERVER-SOCKET-ADDRESS
               BY VALUE LENGTH OF SERVER-SOCKET-ADDRESS
           END-CALL

           DISPLAY "sent: " RETURN-CODE

           STRING "received" DELIMITED BY SPACE, 
           REQUEST-MSG(5:) DELIMITED BY SPACE 
           INTO RECEIVED-FILE-NAME. 

           OPEN OUTPUT RECEIVED-FILE.
           OPEN OUTPUT FAILED-BLOCKS-FILE.

           DISPLAY "Trying to recv"
           
           MOVE 1 TO WAITED-BLOCK-INDEX
           PERFORM RECEIVE-BLOCK.
           
           SUBTRACT 1 FROM TOTAL-BLOCKS
           PERFORM TOTAL-BLOCKS TIMES
               ADD 1 TO WAITED-BLOCK-INDEX
               PERFORM RECEIVE-BLOCK
           END-PERFORM.
           
           CLOSE RECEIVED-FILE.
           CLOSE FAILED-BLOCKS-FILE.
           
      *     STOP RUN.
           
           OPEN I-O FAILED-BLOCKS-FILE.

           PERFORM UNTIL IS-EOF = 1
           MOVE 0 TO FAILED-BLOCK-INDEX
           READ FAILED-BLOCKS-FILE INTO FAILED-BLOCK-INDEX
               AT END MOVE 1 TO IS-EOF
               NOT AT END
                   MOVE 0 TO RECEIVE-SUCCESS
                   PERFORM UNTIL RECEIVE-SUCCESS = 1
                       OPEN I-O RECEIVED-FILE
                       PERFORM REQUEST-FAILED
                       MOVE FAILED-BLOCK-INDEX TO WAITED-BLOCK-INDEX
                       PERFORM RECEIVE-FAILED-BLOCK
                       CLOSE RECEIVED-FILE
                   END-PERFORM
           END-READ
           END-PERFORM.

           
           CLOSE FAILED-BLOCKS-FILE.

           STOP RUN.

           SET-SOCKET-REUSEADDR.

           MOVE 1 TO SOCKET-LEVEL.
           MOVE 1 TO SOCKET-OPTION-LEVEL.
           MOVE 2 TO SOCKET-OPTION-NAME.
           MOVE 1 TO SOCKET-OPTION-VALUE-INT.



           CALL 'setsockopt' USING
               BY VALUE SOCKET-DESCRIPTOR
               BY VALUE SOCKET-OPTION-LEVEL
               BY VALUE SOCKET-OPTION-NAME
               BY REFERENCE SOCKET-OPTION-VALUE-INT
               BY VALUE LENGTH OF SOCKET-OPTION-VALUE-INT
           END-CALL
           DISPLAY "sockopt retradrs: " RETURN-CODE.

           SET-SOCKET-TIMEOUT.

           MOVE 1 TO SOCKET-LEVEL.
           MOVE 1 TO SOCKET-OPTION-LEVEL.
           MOVE 20 TO SOCKET-OPTION-NAME.
           MOVE 5 TO SEC OF SOCKET-OPTION-VALUE-TIMEVAL.
           CALL 'setsockopt' USING
               BY VALUE SOCKET-DESCRIPTOR
               BY VALUE SOCKET-OPTION-LEVEL
               BY VALUE SOCKET-OPTION-NAME
               BY REFERENCE SOCKET-OPTION-VALUE-TIMEVAL
               BY VALUE LENGTH OF SOCKET-OPTION-VALUE-TIMEVAL
           END-CALL
           DISPLAY "sockopt timeout: " RETURN-CODE.


           RECEIVE-BLOCK.

           CALL "recv" USING
               BY VALUE SOCKET-DESCRIPTOR
               BY REFERENCE RECEIVED-MSG
               BY VALUE LENGTH OF RECEIVED-MSG
               BY VALUE 0
           END-CALL.
           
           MOVE 0 TO CALCULATED-CHECKSUM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
               ADD ACTUAL-RECEIVED-MSG(I) TO CALCULATED-CHECKSUM
           END-PERFORM.
               

           
           DISPLAY "RECEIVED:" RECEIVED-MSG-DATA.
           
           DISPLAY CALCULATED-CHECKSUM CHECKSUM.
           IF CALCULATED-CHECKSUM = CHECKSUM AND BLOCK-INDEX = WAITED-BL
      -    OCK-INDEX
               DISPLAY "CHECKSUM IS CORRECT"
               MOVE RECEIVED-MSG-DATA TO FILE-PART
               WRITE FILE-PART
           ELSE
               MOVE SPACES TO FILE-PART
               WRITE FILE-PART
               DISPLAY "WRITE " WAITED-BLOCK-INDEX
               MOVE WAITED-BLOCK-INDEX TO FAILED-BLOCK-NUM
               WRITE FAILED-BLOCK-NUM
           END-IF.

           REQUEST-FAILED.
               
           MOVE SPACES TO REQUEST-MSG
           MOVE FAILED-BLOCK-INDEX TO FAILED-BLOCK-INDEX-STR
           STRING "SUS/" DELIMITED BY SIZE, 
           FAILED-BLOCK-INDEX-STR DELIMITED BY SIZE 
           INTO REQUEST-MSG. 
    
           DISPLAY "SUS rque".
           DISPLAY REQUEST-MSG.
    
           CALL "sendto" USING
               BY VALUE SOCKET-DESCRIPTOR
               BY REFERENCE REQUEST-MSG
               BY VALUE LENGTH OF REQUEST-MSG
               BY VALUE 0
               BY REFERENCE SERVER-SOCKET-ADDRESS
               BY VALUE LENGTH OF SERVER-SOCKET-ADDRESS
           END-CALL.

           RECEIVE-FAILED-BLOCK.

           CALL "recv" USING
               BY VALUE SOCKET-DESCRIPTOR
               BY REFERENCE RECEIVED-MSG
               BY VALUE LENGTH OF RECEIVED-MSG
               BY VALUE 0
           END-CALL.
           
           MOVE 0 TO CALCULATED-CHECKSUM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
               ADD ACTUAL-RECEIVED-MSG(I) TO CALCULATED-CHECKSUM
           END-PERFORM.
               

           
           DISPLAY "RECEIVED:" RECEIVED-MSG-DATA.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WAITED-BLOCK-INDEX
               READ RECEIVED-FILE
               display "advancing"
           END-PERFORM.
           
           DISPLAY CALCULATED-CHECKSUM CHECKSUM.
           IF CALCULATED-CHECKSUM = CHECKSUM AND BLOCK-INDEX = WAITED-BL
      -    OCK-INDEX
               DISPLAY "CHECKSUM IS CORRECT"
               MOVE RECEIVED-MSG-DATA TO FILE-PART
               SUBTRACT 1 FROM FAILED-BLOCK-INDEX
               REWRITE FILE-PART
               ADD 1 TO FAILED-BLOCK-INDEX
               MOVE 1 TO RECEIVE-SUCCESS
           ELSE
               MOVE 0 TO RECEIVE-SUCCESS
           END-IF.