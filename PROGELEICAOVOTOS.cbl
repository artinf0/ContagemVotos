******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELAPURACAO.
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT                    SECTION.

       FILE-CONTROL.

       SELECT ARQELEICAO ASSIGN  TO DISK
              ORGANIZATION       IS LINE SEQUENTIAL.
      *
       SELECT ARQRELAPURACAOELEITORES ASSIGN  TO DISK
              ORGANIZATION       IS LINE SEQUENTIAL.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD  ARQELEICAO
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQELEICAO.TXT".
       01  REG-LINHA.
           03 FD-TITULO       PIC 9(03).
           03 FD-NOME         PIC X(10).
           03 FD-VOTO         PIC 9(03).


       FD  ARQRELAPURACAOELEITORES
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ARQRELAPURACAOTOTAL.TXT".
       01  REG-RELATORIO          PIC X(80).
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  WS-EOF        PIC X(01) VALUE ' '.
       77  WS-LINHABRACO PIC X(80) VALUE SPACES.
       77  WS-PONTILHADO PIC X(80) VALUE ALL '-'.
       77  WS-PAG-AUX    PIC 9(03) VALUE ZEROS.
       77  CONT-LIN      PIC 9(03) VALUE ZEROS.
       77  CONT-LIN-AUX  PIC 9(03) VALUE ZEROS.
       77  WS-VOTO-AUX   PIC 9(03)v99.

       01  WS-VARIAVEIS.
           03 WS-DATA-SISTEMA    PIC X(10).
           03 WS-HORA-SISTEMA    PIC X(08).
           03 WS-DATA-HORA       PIC X(30).
           03 WS-TIMESTAMP.
               05 WS-DATA.
                   07 WS-ANO     PIC 9(04).
                   07 WS-MES     PIC 9(02).
                   07 WS-DIA     PIC 9(02).
               05 WS-HORA.
                   07 WS-HH      PIC 9(02).
                   07 WS-MM      PIC 9(02).
                   07 WS-SS      PIC 9(02).
                   07 WS-MS      PIC 9(02).

       01  WS-CABECALHO.
           03 WS-LINHA1.
              05 FILLER         PIC X(15) VALUE 'CURSO COBOL - '.
              05 FILLER         PIC x(6) VALUE 'DATA:'.
              05 WS-DATACAB     PIC x(10) VALUE ZEROS.
              05 FILLER         PIC x(6) VALUE ' HORA:'.
              05 WS-HORACAB PIC x(10) VALUE ZEROS.
              05 FILLER         PIC X(20) VALUE SPACES.
              05 FILLER         PIC X(8) VALUE 'PAG:'.
              05 WS-PAG         PIC zzz9.

           03 WS-LINHA2.
              05 FILLER  PIC X(28) VALUE SPACES.
              05 FILLER  PIC X(26) VALUE 'Listagem de Votos Apurados'.
              05 FILLER  PIC X(26) VALUE SPACES.

           03 WS-LINHA3.
              05 FILLER  PIC X(14) VALUE 'Titulo Eleitor'.
              05 FILLER  PIC X(20) VALUE SPACES.
              05 FILLER  PIC X(14) VALUE 'Nome Eleitor'.
              05 FILLER  PIC X(22) VALUE SPACES.
              05 FILLER  PIC X(16) VALUE 'Voto'.

       01  WS-LINHA-BRANCO.
           03 FILLER     PIC X(80) VALUE SPACES.

       01  WS-DETALHE.
           03 WS-TITULO  PIC 9(03) VALUE ZEROS.
           03 FILLER     PIC X(31) VALUE SPACES.
           03 WS-NOME    PIC X(10) VALUE 'T'.
           03 FILLER     PIC X(26) VALUE SPACES.
           03 WS-VOTO    PIC 9(03) VALUE ZEROS.


      *----RODAPE
       01  WS-RODAPE0.
           05 FILLER  PIC X(07) VALUE SPACES.
           05 FILLER  PIC X(14) VALUE 'Numero Partido'.
           05 FILLER  PIC X(02) VALUE SPACES.
           05 FILLER  PIC X(17) VALUE 'Nome do Candidato'.
           05 FILLER  PIC X(04) VALUE SPACES.
           05 FILLER  PIC X(14) VALUE 'Total de Votos'.
           05 FILLER  PIC X(22) VALUE SPACES.
       01  WS-RODAPE1.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 001.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'THULIO'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-1     PIC 9(03) VALUE ZEROS.
       01  WS-RODAPE2.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 002.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'JOAOZINHO'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-2     PIC 9(03) VALUE ZEROS.
       01  WS-RODAPE3.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 003.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'ANA'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-3     PIC 9(03) VALUE ZEROS.
       01  WS-RODAPE4.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 004.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'JOANA'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-4     PIC 9(03) VALUE ZEROS.
       01  WS-RODAPE5.
           03 FILLER       PIC X(07) VALUE SPACES.
           03 FILLER       PIC 9(03) VALUE 005.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(17) VALUE 'CARLOS'.
           03 FILLER       PIC X(04) VALUE SPACES.
           03 WS-CNT-5     PIC 9(03) VALUE ZEROS.
       01  WS-RODAPE6.
           03 FILLER         PIC X(20) VALUE 'TOTAL DE ELEITORES: '.
           03 WS-TOTELEITOR  PIC 9(04) VALUE ZEROS.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
           MAIN-PROCEDURE.
           OPEN INPUT ARQELEICAO
           OPEN OUTPUT ARQRELAPURACAOELEITORES

           PERFORM 1000-LER-ARQUIVO
           PERFORM 1200-TRATA-DATAHORA

           MOVE 70 TO CONT-LIN

            PERFORM UNTIL WS-EOF = 'S'
               IF CONT-LIN >= 26
                 PERFORM 2000-TRATA-CABECALHO
               END-IF
               PERFORM 3000-TRATA-DETALHE
               PERFORM 1000-LER-ARQUIVO
            END-PERFORM

           PERFORM 3000-TRATA-DETALHE
           PERFORM 4000-TRATA-RODAPE

           CLOSE ARQELEICAO ARQRELAPURACAOELEITORES
           STOP RUN.
      *----------------------------------------------------------------*
       1000-LER-ARQUIVO.
      *----------------------------------------------------------------*
           READ ARQELEICAO
                  AT END
                     MOVE 'S' TO WS-EOF
                  NOT AT END
                     ADD 1 TO WS-TOTELEITOR
           END-READ
           .
           EXIT.
      *----------------------------------------------------------------*
       1200-TRATA-DATAHORA.
      *----------------------------------------------------------------*
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           DISPLAY 'WS-TIMESTAMP: ' WS-TIMESTAMP

           STRING WS-DIA '/' WS-MES '/' WS-ANO
               DELIMITED BY SIZE INTO WS-DATA-SISTEMA
           END-STRING
           DISPLAY 'DATA SISTEMA: ' WS-DATA-SISTEMA
           MOVE WS-DATA-SISTEMA TO WS-DATACAB

           STRING WS-HH ':' WS-MM ':' WS-SS
               DELIMITED BY SIZE INTO WS-HORA-SISTEMA
           END-STRING
           DISPLAY 'HORA SISTEMA: ' WS-HORA-SISTEMA
           MOVE WS-HORA-SISTEMA TO WS-HORACAB
           EXIT.
      *----------------------------------------------------------------*
       2000-TRATA-CABECALHO.
      *----------------------------------------------------------------*
           ADD 1 TO WS-PAG-AUX
           MOVE WS-PAG-AUX TO WS-PAG
           display WS-PAG
           IF CONT-LIN-AUX > 1
           WRITE REG-RELATORIO FROM WS-LINHA-BRANCO
           END-IF
           WRITE REG-RELATORIO FROM WS-LINHA1
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-LINHA2
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-LINHA3
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           MOVE 6 TO CONT-LIN
           .
           EXIT.
      *----------------------------------------------------------------*
       3000-TRATA-DETALHE.
      *----------------------------------------------------------------*
           MOVE FD-TITULO  TO WS-TITULO
           MOVE FD-NOME    TO WS-NOME
           MOVE FD-VOTO    TO WS-VOTO
           WRITE REG-RELATORIO FROM WS-DETALHE
           ADD 1 TO CONT-LIN
           ADD 1 TO CONT-LIN-AUX

           IF   FD-VOTO =   001
               ADD 1 TO WS-CNT-1
           ELSE IF   FD-VOTO =   002
                     ADD 1 TO WS-CNT-2
           ELSE IF   FD-VOTO = 003
                     ADD 1 TO WS-CNT-3
           ELSE IF   FD-VOTO = 004
                     ADD 1 TO WS-CNT-4
           ELSE IF   FD-VOTO = 005
                     ADD 1 TO WS-CNT-5
           ELSE IF   FD-VOTO = 000
                     DISPLAY 'VOTO EM BRANCO'
           END-IF


           IF CONT-LIN = 20
             PERFORM 2000-TRATA-CABECALHO.
           EXIT.
      *----------------------------------------------------------------*
       4000-TRATA-RODAPE.
      *----------------------------------------------------------------*
           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-RODAPE0
           WRITE REG-RELATORIO FROM WS-RODAPE0
           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-RODAPE1
           WRITE REG-RELATORIO FROM WS-RODAPE2
           WRITE REG-RELATORIO FROM WS-RODAPE3
           WRITE REG-RELATORIO FROM WS-RODAPE4
           WRITE REG-RELATORIO FROM WS-RODAPE5
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-RODAPE6
           WRITE REG-RELATORIO FROM WS-PONTILHADO
           .
           EXIT.

       END PROGRAM RELAPURACAO.
