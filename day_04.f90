PROGRAM main

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), PARAMETER :: ex_file = "inputs/day_04_example.txt"
    CHARACTER(LEN=*), PARAMETER :: in_file = "inputs/day_04_inputs.txt"
    CHARACTER(LEN=32) :: file
    LOGICAL :: test
    !=============================================================================================!
    
    test = .FALSE.
    IF(test) THEN
        file = ex_file
    ELSE
        file = in_file
    ENDIF

    CALL count_xmas(TRIM(file))


END PROGRAM main


SUBROUTINE count_xmas(filename)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN) :: filename

    CHARACTER, DIMENSION(:, :), ALLOCATABLE :: word_search
    CHARACTER(LEN=512) :: form
    CHARACTER(LEN=4)   :: word
    CHARACTER(LEN=3)   :: diag_r
    CHARACTER(LEN=3)   :: diag_l
    CHARACTER :: chr
    INTEGER   :: f_unit
    INTEGER   :: io_stat
    INTEGER   :: line_length
    INTEGER   :: num_lines
    INTEGER   :: xmas_count
    INTEGER   :: i, j, k
    !=============================================================================================!
    
    OPEN(file=filename, action="read", status="old", newunit=f_unit)
    
    !--------------------------------------------- Determine size of input and allocate data -----!
    line_length = 0
    DO
        READ(f_unit, "(A)", iostat=io_stat, advance="no") chr
        IF(io_stat /= 0) EXIT
        line_length = line_length + 1
    ENDDO

    REWIND(f_unit)
    
    num_lines = 0
    DO
        READ(f_unit, "(A)", iostat=io_stat) chr
        IF(io_stat /= 0) EXIT
        num_lines = num_lines + 1
    ENDDO

    WRITE(*, *) "SIZE OF INPUT:", line_length, "x", num_lines

    REWIND(f_unit)

    ALLOCATE(word_search(num_lines, line_length))
    !--------------------------------------------- Determine size of input and allocate data -----!
    
    !-------------------------------------------------------------------------- Read in data -----!
    form = "("
    DO i = 1, line_length
        form = TRIM(form) // "A1,"
    ENDDO
    form = TRIM(form) // ")"

    DO i = 1, num_lines
        READ(f_unit, TRIM(form), iostat=io_stat) word_search(i, :)
        IF(io_stat /= 0) EXIT
    ENDDO
    !-------------------------------------------------------------------------- Read in data -----!
    
    !---------------------------------------------------------------------------- Count XMAS -----!
    xmas_count = 0
    HORIZONTAL: DO i = 1, num_lines
        DO j = 1, line_length - 3
            DO k = 1, 4
                word(k:k) = word_search(i, j + k - 1)
            ENDDO

            IF(word == "XMAS" .OR. word == "SAMX") xmas_count = xmas_count + 1
        ENDDO
    ENDDO HORIZONTAL

    VERTICAL: DO i = 1, line_length
        DO j = 1, num_lines - 3
            DO k = 1, 4
                word(k:k) = word_search(j + k - 1, i)
            ENDDO

            IF(word == "XMAS" .OR. word == "SAMX") xmas_count = xmas_count + 1
        ENDDO
    ENDDO VERTICAL

    DIAGONAL_LEFT: DO i = 1, line_length - 3
        DO j = 1, num_lines - 3
            DO k = 1, 4
                word(k:k) = word_search(j + k - 1, i + k - 1)
            ENDDO

            IF(word == "XMAS" .OR. word == "SAMX") xmas_count = xmas_count + 1
        ENDDO
    ENDDO DIAGONAL_LEFT

    DIAGONAL_RIGHT: DO i = 4, line_length
        DO j = 1, num_lines - 3
            DO k = 1, 4
                word(k:k) = word_search(j + k - 1, i - k + 1)
            ENDDO

            IF(word == "XMAS" .OR. word == "SAMX") xmas_count = xmas_count + 1
        ENDDO
    ENDDO DIAGONAL_RIGHT

    WRITE(*, *) "XMAS-COUNT:", xmas_count
    !---------------------------------------------------------------------------- Count XMAS -----!
    
    !--------------------------------------------------------------------------- Count X-MAS -----!
    xmas_count = 0
    DO i = 2, num_lines - 1
        DO j = 2, line_length - 1
            IF(word_search(i, j) == "A") THEN
                diag_r = word_search(i - 1, j - 1) // "A" // word_search(i + 1, j + 1)
                diag_l = word_search(i - 1, j + 1) // "A" // word_search(i + 1, j - 1)
                
                IF(        (diag_r == "SAM" .OR. diag_r == "MAS") &
                   & .AND. (diag_l == "SAM" .OR. diag_l == "MAS") ) xmas_count = xmas_count + 1
            ENDIF
        ENDDO
    ENDDO

    WRITE(*, *) "X-MAS COUNT:", xmas_count
    !--------------------------------------------------------------------------- Count X-MAS -----!

    DEALLOCATE(word_search)
    CLOSE(f_unit)

END SUBROUTINE count_xmas
