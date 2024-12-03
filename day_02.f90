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

    WRITE(*, *) ""
    WRITE(*, *) "--- DAY 2 - Part II"
    WRITE(*, *) ""

    CALL count_safe_reports_with_tolerance(TRIM(file))

    WRITE(*, *) ""
    WRITE(*, *) "--- END DAY 2 ---"
    WRITE(*, *) ""

END PROGRAM main


SUBROUTINE count_safe_reports(filename)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN) :: filename

    INTEGER                            :: f_unit
    INTEGER                            :: io_stat
    INTEGER                            :: count
    INTEGER                            :: safe_count
    INTEGER                            :: i
    INTEGER, DIMENSION(:), ALLOCATABLE :: levels
    CHARACTER(LEN=100)                 :: line

    INTERFACE
        LOGICAL FUNCTION check_safe(report, num_levels)
            INTEGER, INTENT(IN)                        :: num_levels
            INTEGER, DIMENSION(num_levels), INTENT(IN) :: report
        END FUNCTION check_safe
    END INTERFACE
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
        
        IF(check_safe(levels, count)) safe_count = safe_count + 1

        DEALLOCATE(levels)
    ENDDO

    WRITE(*, *) "NUMBER OF SAFE REPORTS:", safe_count

    CLOSE(f_unit)

END SUBROUTINE count_safe_reports


SUBROUTINE count_safe_reports_with_tolerance(filename)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN) :: filename

    INTEGER                            :: f_unit
    INTEGER                            :: io_stat
    INTEGER                            :: count
    INTEGER                            :: i, j
    INTEGER                            :: safe_count
    CHARACTER(LEN=100)                 :: line
    INTEGER, DIMENSION(:), ALLOCATABLE :: report
    INTEGER, DIMENSION(:), ALLOCATABLE :: tol_report

    INTERFACE
        LOGICAL FUNCTION check_safe(report, num_levels)
            INTEGER, INTENT(IN)                        :: num_levels
            INTEGER, DIMENSION(num_levels), INTENT(IN) :: report
        END FUNCTION check_safe
    END INTERFACE
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

        ALLOCATE(report(count))
        READ(line, *) report
        
        IF(check_safe(report, count)) THEN
            safe_count = safe_count + 1
        ELSE
            !--- Tolerance check:
            ALLOCATE(tol_report(count - 1))
            
            DO i = 1, count
                DO j = 1, count - 1
                    IF(j < i) THEN
                        tol_report(j) = report(j)
                    ELSE
                        tol_report(j) = report(j + 1)
                    ENDIF
                ENDDO

                IF(check_safe(tol_report, count - 1)) THEN
                    safe_count = safe_count + 1
                    EXIT
                ENDIF
            ENDDO

            DEALLOCATE(tol_report)
        ENDIF

        DEALLOCATE(report)
    ENDDO

    WRITE(*, *) "NUMBER OF SAFE REPORTS (tolerance):", safe_count

    CLOSE(f_unit)

END SUBROUTINE count_safe_reports_with_tolerance


LOGICAL FUNCTION check_safe(report, num_levels)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, INTENT(IN)                        :: num_levels
    INTEGER, DIMENSION(num_levels), INTENT(IN) :: report

    LOGICAL :: increasing
    INTEGER :: index
    INTEGER :: diff
    !=============================================================================================!
    
    increasing = (report(1) - report(2) > 0)
    check_safe = .TRUE.

    DO index = 1, num_levels - 1
        diff = report(index) - report(index + 1)

        IF(diff == 0 .OR. ABS(diff) > 3) THEN
            check_safe = .FALSE.
            EXIT
        ENDIF

        IF(diff < 0 .AND. increasing) THEN
            check_safe = .FALSE.
            EXIT
        ENDIF

        IF(diff > 0 .AND. .NOT. increasing) THEN
            check_safe = .FALSE.
            EXIT
        ENDIF
    ENDDO

END FUNCTION check_safe
