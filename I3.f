c******************************************************************************
c
c      Non-graphical intrface routines likely to be platform-specific
c
c******************************************************************************
c
       SUBROUTINE SOWSZZ
c
c      Sets output window scroll
c------------------------------------------------------------------------------
c    no action
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CLOWZZ
c
c      Clears output window
c------------------------------------------------------------------------------
c    no action
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE CLPWZZ
c
c      Clears the prompt window
c------------------------------------------------------------------------------
c    no action
       RETURN
       END
c******************************************************************************
c
       SUBROUTINE POSTZZ(LINE,N,PROMPT)
c
c      Writes N characters of LINE to screen.
c      If PROMPT = .TRUE., leaves cursor at end of line for data read.
c      Used in displaying internal files.
c------------------------------------------------------------------------------
       CHARACTER*1 LINE(N)
       LOGICAL PROMPT
c------------------------------------------------------------------------------
       IF (PROMPT)  THEN
               WRITE (*,2) LINE
2              FORMAT ($,80A1)
           ELSE
               WRITE (*,4) LINE
4              FORMAT(80A1)
           ENDIF
       RETURN
       END
c******************************************************************************

