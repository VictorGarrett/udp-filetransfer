       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROGRAM-SERVER.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO SELECTED-FILE-NAME
           ORGANIZATION IS SEQUENTIAL.
      *     FILE STATUS IS F-STATUS.

           

       DATA DIVISION.
       
       FILE SECTION.
       FD INPUT-FILE.
       01  FILE-PART.
       03  ACTUAL-FILE-PART BINARY-CHAR OCCURS 50.


       WORKING-STORAGE SECTION.
       01  F-STATUS PIC XX.
       01  IS-EOF BINARY-SHORT VALUE 0.
       01  SELECTED-FILE-NAME PIC X(20).
       
      *socket creation
       01  SOCKET-NAMESPACE BINARY-LONG VALUE 2.
       01  SOCKET-STYLE BINARY-LONG VALUE 2.
       01  SOCKET-PROTOCOL BINARY-LONG VALUE 0.
       01  SOCKET-DESCRIPTOR BINARY-LONG.

      *socket option creation
       01  SOCKET-LEVEL BINARY-LONG VALUE 1.

       01  SOCKET-OPTION-LEVEL BINARY-LONG VALUE 1.
       01  SOCKET-OPTION-NAME BINARY-LONG VALUE 2.
       01  SOCKET-OPTION-VALUE BINARY-LONG VALUE 1.

      *socket address for binding
       01 SOCKET-RECEIVE-ADDRESS.
            03  FAMILY BINARY-SHORT VALUE 2.
            03  PORT BINARY-SHORT VALUE 8080.
            03  IP BINARY-LONG VALUE 0.
            03  SIN-ZERO BINARY-CHAR OCCURS 8.
      *socket that has sent a message
       01 CLIENT-SOCKET-ADDRESS.
            03  FAMILY BINARY-SHORT VALUE 2.
            03  PORT BINARY-SHORT VALUE 8080.
            03  IP BINARY-LONG VALUE 10.
            03  SIN-ZERO BINARY-CHAR OCCURS 8.
       01 CLIENT-SOCKET-SIZE BINARY-LONG.

 
       01  RECEIVED-MSG.
           03 MESSAGE-CONTENT PIC X(112).


       01  RESPONSE-MSG.
           03  BLOCK-INDEX BINARY-DOUBLE.
           03  TOTAL-BLOCKS BINARY-DOUBLE. 
           03  RESPONSE-MSG-DATA.
               05  ACTUAL-RESPONSE-MSG BINARY-CHAR OCCURS 50.
           03  CHECKSUM BINARY-DOUBLE.


       01  BLOCK-TO-RESEND BINARY-LONG.
       01  I BINARY-LONG.
        
       PROCEDURE DIVISION.
           
           CALL 'socket' USING
               BY VALUE SOCKET-NAMESPACE
               BY VALUE SOCKET-STYLE
               BY VALUE SOCKET-PROTOCOL
               GIVING SOCKET-DESCRIPTOR
           END-CALL
           DISPLAY "SOCKET-DESCRIPTOR: " SOCKET-DESCRIPTOR
        
           CALL 'setsockopt' USING
               BY VALUE SOCKET-DESCRIPTOR
               BY VALUE SOCKET-OPTION-LEVEL
               BY VALUE SOCKET-OPTION-NAME
               BY REFERENCE SOCKET-OPTION-VALUE
               BY VALUE LENGTH OF SOCKET-OPTION-VALUE
           END-CALL
           DISPLAY "sockopt: " RETURN-CODE

           CALL 'htons' USING
               BY VALUE 8080
               GIVING PORT OF SOCKET-RECEIVE-ADDRESS 
           END-CALL

           CALL "bind" USING 
               BY VALUE SOCKET-DESCRIPTOR
               BY REFERENCE SOCKET-RECEIVE-ADDRESS
               BY VALUE LENGTH OF SOCKET-RECEIVE-ADDRESS
           END-CALL

           DISPLAY "bind: " RETURN-CODE


           PERFORM UNTIL EXIT
           MOVE SPACES TO RECEIVED-MSG

           MOVE LENGTH OF CLIENT-SOCKET-ADDRESS TO CLIENT-SOCKET-SIZE
           CALL "recvfrom" USING
               BY VALUE SOCKET-DESCRIPTOR
               BY REFERENCE RECEIVED-MSG
               BY VALUE LENGTH OF RECEIVED-MSG
               BY VALUE 0
               BY REFERENCE CLIENT-SOCKET-ADDRESS
               BY REFERENCE CLIENT-SOCKET-SIZE
           END-CALL
           DISPLAY "recv: " RETURN-CODE
           DISPLAY "FROM " IP OF CLIENT-SOCKET-ADDRESS   

           DISPLAY "recv msg: " MESSAGE-CONTENT

           IF MESSAGE-CONTENT(1:4) = "GET/"
               MOVE MESSAGE-CONTENT(5:) TO SELECTED-FILE-NAME
               PERFORM SEND-FILE        
           END-IF

           IF  MESSAGE-CONTENT(1:4) = "SUS/"
               MOVE MESSAGE-CONTENT(5:) TO BLOCK-TO-RESEND
               PERFORM RESEND-FILE-BLOCK     
           END-IF
           END-PERFORM.
           STOP RUN.


           SEND-FILE.

           
           OPEN INPUT INPUT-FILE.

           PERFORM UNTIL IS-EOF = 1
           MOVE SPACES TO FILE-PART
           READ INPUT-FILE
              AT END MOVE 1 TO IS-EOF
              NOT AT END
              ADD 1 TO TOTAL-BLOCKS
           END-READ
           END-PERFORM.

           CLOSE INPUT-FILE.

           MOVE 0 TO IS-EOF.
           OPEN INPUT INPUT-FILE.

           MOVE 1 TO BLOCK-INDEX OF RESPONSE-MSG
           PERFORM UNTIL IS-EOF = 1
           MOVE SPACES TO FILE-PART
           READ INPUT-FILE INTO RESPONSE-MSG-DATA OF RESPONSE-MSG
              AT END MOVE 1 TO IS-EOF
              NOT AT END
               
              MOVE 0 TO CHECKSUM

              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
                 ADD ACTUAL-RESPONSE-MSG(I) TO CHECKSUM
              END-PERFORM

              IF BLOCK-INDEX = 2
                 MOVE 0 TO ACTUAL-RESPONSE-MSG(2)
                 MOVE 1 TO ACTUAL-RESPONSE-MSG(3)
                 MOVE 3 TO ACTUAL-RESPONSE-MSG(4)
              END-IF

              DISPLAY "SENDING: " RESPONSE-MSG-DATA
              CALL "sendto" USING
                BY VALUE SOCKET-DESCRIPTOR
                BY REFERENCE RESPONSE-MSG
                BY VALUE LENGTH OF RESPONSE-MSG
                BY VALUE 0
                BY REFERENCE CLIENT-SOCKET-ADDRESS
                BY VALUE CLIENT-SOCKET-SIZE
              END-CALL
              ADD 1 TO BLOCK-INDEX OF RESPONSE-MSG
               
           END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

           RESEND-FILE-BLOCK. 

           MOVE 0 TO IS-EOF.
           OPEN INPUT INPUT-FILE.

           MOVE 1 TO BLOCK-INDEX OF RESPONSE-MSG
           PERFORM UNTIL IS-EOF = 1

       
           MOVE SPACES TO FILE-PART
          
           READ INPUT-FILE INTO RESPONSE-MSG-DATA OF RESPONSE-MSG
              AT END MOVE 1 TO IS-EOF
              NOT AT END

              IF BLOCK-INDEX = BLOCK-TO-RESEND 
                MOVE 0 TO CHECKSUM
  
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
                   ADD ACTUAL-RESPONSE-MSG(I) TO CHECKSUM
                END-PERFORM
  
                DISPLAY "SENDING: " RESPONSE-MSG-DATA
                CALL "sendto" USING
                  BY VALUE SOCKET-DESCRIPTOR
                  BY REFERENCE RESPONSE-MSG
                  BY VALUE LENGTH OF RESPONSE-MSG
                  BY VALUE 0
                  BY REFERENCE CLIENT-SOCKET-ADDRESS
                  BY VALUE CLIENT-SOCKET-SIZE
                END-CALL
              END-IF
              ADD 1 TO BLOCK-INDEX OF RESPONSE-MSG
               
           END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.