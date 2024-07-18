       IDENTIFICATION DIVISION.
       PROGRAM-ID. LoanManagement.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CLIENTS
           ASSIGN TO 'clients.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLIENT-ID.
           SELECT LOANS ASSIGN TO 'loans.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-ID.

       DATA DIVISION.
       FILE SECTION.

       FD  CLIENTS.
       01  CLIENT-REC.
           05  CLIENT-ID          PIC 9(5).
           05  CLIENT-NAME        PIC X(30).
           05  CLIENT-ADDRESS     PIC X(50).
           05  CLIENT-PHONE       PIC X(15).

       FD  LOANS.
       01  LOAN-REC.
           05  LOAN-ID            PIC 9(5).
           05  LOAN-CLIENT-ID     PIC 9(5).
           05  LOAN-AMOUNT        PIC 9(7)V99.
           05  LOAN-INTEREST      PIC 9(3)V99.
           05  LOAN-DATE          PIC 9(8).

       WORKING-STORAGE SECTION.
       01  WS-MENU-OPTION         PIC 9 VALUE 0.
       01  WS-CLIENT-ID           PIC 9(5) VALUE ZEROS.
       01  WS-LOAN-ID             PIC 9(5) VALUE ZEROS.
       01  EOF-CLIENT             PIC X VALUE 'N'.
       01  EOF-LOAN               PIC X VALUE 'N'.
       01  CLIENT-FOUND           PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM UNTIL WS-MENU-OPTION = 9
               DISPLAY '1. Add Client'
               DISPLAY '2. Add Loan to Client'
               DISPLAY '3. View Records'
               DISPLAY '9. Exit'
               ACCEPT WS-MENU-OPTION
               EVALUATE WS-MENU-OPTION
                   WHEN 1
                       PERFORM ADD-CLIENT
                   WHEN 2
                       PERFORM ADD-LOAN-TO-CLIENT
                   WHEN 3
                       PERFORM VIEW-RECORDS
                   WHEN 9
                       DISPLAY 'Exiting the program.'
                   WHEN OTHER
                       DISPLAY 'Invalid option.'
               END-EVALUATE
           END-PERFORM.
           STOP RUN.

       ADD-CLIENT.
           OPEN I-O CLIENTS.
           DISPLAY 'Enter Client ID:'
           ACCEPT CLIENT-ID.
           DISPLAY 'Enter Client Name:'
           ACCEPT CLIENT-NAME.
           DISPLAY 'Enter Client Address:'
           ACCEPT CLIENT-ADDRESS.
           DISPLAY 'Enter Client Phone:'
           ACCEPT CLIENT-PHONE.
           WRITE CLIENT-REC INVALID KEY
               DISPLAY 'Error: Client already exists.'
           END-WRITE.
           CLOSE CLIENTS.
           DISPLAY 'Client added successfully.'
           .

       ADD-LOAN-TO-CLIENT.
           DISPLAY 'Enter Client ID:'
           ACCEPT WS-CLIENT-ID.
           OPEN I-O CLIENTS.
           MOVE 'N' TO CLIENT-FOUND.
           READ CLIENTS KEY IS CLIENT-ID
               INVALID KEY
                   DISPLAY 'Error: Client not found.'
               NOT INVALID KEY
                   MOVE 'Y' TO CLIENT-FOUND
                   DISPLAY 'Client ID: ' CLIENT-ID
                   DISPLAY 'Client Name: ' CLIENT-NAME
                   DISPLAY 'Client Address: ' CLIENT-ADDRESS
                   DISPLAY 'Client Phone: ' CLIENT-PHONE
              CLOSE CLIENTS
           END-READ.

           IF CLIENT-FOUND = 'Y'
               OPEN I-O LOANS
               DISPLAY 'Enter Loan ID:'
               ACCEPT LOAN-ID
               DISPLAY 'Enter Loan Amount:'
               ACCEPT LOAN-AMOUNT
               DISPLAY 'Enter Loan Interest:'
               ACCEPT LOAN-INTEREST
               DISPLAY 'Enter Loan Date (YYYYMMDD):'
               ACCEPT LOAN-DATE
               MOVE WS-CLIENT-ID TO LOAN-CLIENT-ID
               WRITE LOAN-REC INVALID KEY
                   DISPLAY 'Error: Loan already exists.'
               END-WRITE
               CLOSE LOANS
               DISPLAY 'Loan added successfully.'
           ELSE
               DISPLAY 'Cannot add loan. Client not found.'
           END-IF
           .

       VIEW-RECORDS.
           MOVE 'N' TO EOF-CLIENT.
           OPEN I-O CLIENTS.
           DISPLAY 'Clients:'
           PERFORM UNTIL EOF-CLIENT = 'Y'
               READ CLIENTS NEXT RECORD
                   AT END MOVE 'Y' TO EOF-CLIENT
               NOT AT END
                   DISPLAY 'Client ID: ' CLIENT-ID
                   DISPLAY 'Client Name: ' CLIENT-NAME
                   DISPLAY 'Client Address: ' CLIENT-ADDRESS
                   DISPLAY 'Client Phone: ' CLIENT-PHONE
                   DISPLAY '-------------------------'
               END-READ
           END-PERFORM.
           CLOSE CLIENTS.

           MOVE 'N' TO EOF-LOAN.
           OPEN I-O LOANS.
           DISPLAY 'Loans:'
           PERFORM UNTIL EOF-LOAN = 'Y'
               READ LOANS NEXT RECORD
                   AT END MOVE 'Y' TO EOF-LOAN
               NOT AT END
                   DISPLAY 'Loan ID: ' LOAN-ID
                   DISPLAY 'Client ID: ' LOAN-CLIENT-ID
                   DISPLAY 'Loan Amount: ' LOAN-AMOUNT
                   DISPLAY 'Loan Interest: ' LOAN-INTEREST
                   DISPLAY 'Loan Date: ' LOAN-DATE
                   DISPLAY '-------------------------'
               END-READ
           END-PERFORM.
           CLOSE LOANS.
           DISPLAY 'Records displayed successfully.'
           .
