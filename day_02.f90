PROGRAM main

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), PARAMETER :: example_file = "./inputs/day_02_example.txt"
    CHARACTER(LEN=*), PARAMETER :: input_file   = "./inputs/day_02_inputs.txt"
    CHARACTER(LEN=50)           :: file
    LOGICAL                     :: test

    !=============================================================================================!

    WRITE(*, *) ""
    WRITE(*, *) "=========================="
    WRITE(*, *) "===== ADVENT OF CODE ====="
    WRITE(*, *) "=========================="
    WRITE(*, *) ""
    WRITE(*, *) "--- DAY 2 - Part I"
    WRITE(*, *) ""

    test = .FALSE.

    IF(test) THEN
        file = example_file
    ELSE
        file = input_file
    ENDIF

    CALL count_safe_reports(TRIM(file))

END PROGRAM main


SUBROUTINE count_safe_reports(filename)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN) :: filename

    INTEGER                            :: f_unit
    INTEGER                            :: io_stat
    INTEGER                            :: count
    INTEGER                            :: safe_count
    INTEGER                            :: n1, n2, diff
    INTEGER                            :: i
    INTEGER, DIMENSION(:), ALLOCATABLE :: levels
    CHARACTER(LEN=100)                 :: line
    LOGICAL                            :: increasing
    LOGICAL                            :: is_safe
    !=============================================================================================!
    
    OPEN(file=filename, action="read", status="old", newunit=f_unit)
    
    safe_count = 0
    DO
        READ(f_unit, "(A)", iostat=io_stat) line
        IF(io_stat /= 0) EXIT
        
        count = 0
        DO i = 1, LEN_TRIM(line)
            IF(line(i:i) .EQ. " ") count = count + 1
        ENDDO
        count = count + 1

        ALLOCATE(levels(count))

        READ(line, *) levels(:)
        
        IF(levels(1) - levels(2) > 0) THEN
            increasing = .FALSE.
        ELSE
            increasing = .TRUE.
        ENDIF
        
        is_safe = .TRUE.
        CHECK_SAFE: DO i = 1, count - 1
            n1 = levels(i)
            n2 = levels(i + 1)

            diff = n1 - n2

            IF(diff == 0 .OR. ABS(diff) > 3)    is_safe = .FALSE.
            IF(diff > 0 .AND. increasing)       is_safe = .FALSE.
            IF(diff < 0 .AND. .NOT. increasing) is_safe = .FALSE.
        ENDDO CHECK_SAFE

        IF(is_safe) THEN
            safe_count = safe_count + 1
            WRITE(*, *) "SAFE!", levels(:)
        ENDIF

        DEALLOCATE(levels)
    ENDDO

    WRITE(*, *) "NUMBER OF SAFE REPORTS:", safe_count

    CLOSE(f_unit)

END SUBROUTINE count_safe_reports
