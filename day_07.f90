PROGRAM main

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), PARAMETER :: ex_file = "inputs/day_07_example.txt"
    CHARACTER(LEN=*), PARAMETER :: in_file = "inputs/day_07_inputs.txt"
    CHARACTER(LEN=32)           :: file
    LOGICAL                     :: test
    !=============================================================================================!
    
    test = .FALSE.
    IF(      test) file = ex_file
    IF(.NOT. test) file = in_file
    
    CALL calc_total_calibration(TRIM(file))

END PROGRAM main


SUBROUTINE calc_total_calibration(filename)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN) :: filename

    INTEGER          :: f_unit
    INTEGER          :: io_stat
    INTEGER          :: sep_index
    INTEGER          :: num_values
    INTEGER          :: i
    INTEGER(KIND=16) :: res
    INTEGER(KIND=16) :: total_calibration
    INTEGER, DIMENSION(:), ALLOCATABLE :: values
    CHARACTER(LEN=256) :: line

    INTERFACE
        LOGICAL FUNCTION check_calibration(res, values)
            INTEGER(KIND=16), INTENT(IN)      :: res
            INTEGER, DIMENSION(:), INTENT(IN) :: values
        END FUNCTION check_calibration
    END INTERFACE
    !=============================================================================================!

    OPEN(file=filename, action="read", status="old", newunit=f_unit)
    
    total_calibration = 0

    DO
        READ(f_unit, "(A)", iostat=io_stat) line
        IF(io_stat /= 0) EXIT
        
        num_values = 0

        DO i = 1, LEN_TRIM(line)
            IF(line(i:i) == ":") THEN
                READ(line(:i - 1), *) res
                sep_index = i
            ELSEIF(line(i:i) == " ") THEN
                num_values = num_values + 1
            ENDIF
        ENDDO

        ALLOCATE(values(num_values))

        READ(line(sep_index + 1:), *) values(:)
        
        WRITE(*, *) REPEAT("-", 50)
        WRITE(*, *) "RESULT:", res
        WRITE(*, *) "VALUES:", values(:)
        
        IF(check_calibration(res, values)) total_calibration = total_calibration + res

        DEALLOCATE(values)
    ENDDO

    CLOSE(f_unit)

    WRITE(*, *) "TOTAL CALIBRATION:", total_calibration

END SUBROUTINE calc_total_calibration


LOGICAL FUNCTION check_calibration(res, values)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER(KIND=16), INTENT(IN)      :: res
    INTEGER, DIMENSION(:), INTENT(IN) :: values

    INTEGER, DIMENSION(SIZE(values) - 1) :: operators
    INTEGER                              :: i
    INTEGER(KIND=16)                     :: test_res

    INTERFACE
        FUNCTION concat(left, right)
            INTEGER(KIND=16)             :: concat
            INTEGER(KIND=16), INTENT(IN) :: left, right
        END FUNCTION concat
    END INTERFACE
    !=============================================================================================!
    
    operators(:) = 0
    check_calibration = .FALSE.

    DO
        test_res = values(1)

        DO i = 1, SIZE(operators)
            SELECT CASE(operators(i))
                CASE(0)
                    test_res = test_res + values(i + 1)
                CASE(1)
                    test_res = test_res * values(i + 1)
                CASE(2)
                    test_res = concat(test_res, INT(values(i + 1), 16))
            END SELECT
        ENDDO

        IF(test_res == res) THEN
            check_calibration = .TRUE.
            EXIT
        ENDIF
        
        IF(ALL(operators == 2)) EXIT

        operators(1) = operators(1) + 1
        DO i = 1, SIZE(operators) - 1
            IF(operators(i) > 2) THEN
                operators(i    ) = 0
                operators(i + 1) = operators(i + 1) + 1
            ELSE
                EXIT
            ENDIF
        ENDDO
    ENDDO

    WRITE(*, *) "TEST:", check_calibration

END FUNCTION check_calibration


FUNCTION concat(left, right)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER(KIND=16)             :: concat
    INTEGER(KIND=16), INTENT(IN) :: left, right

    CHARACTER(LEN=256) :: left_str, right_str
    CHARACTER(LEN=256) :: short_number
    INTEGER :: i, j
    !=============================================================================================!
    
    WRITE(left_str , "(I256)") left
    WRITE(right_str, "(I256)") right
    
    short_number = TRIM(ADJUSTL(left_str)) // TRIM(ADJUSTL(right_str))

    !WRITE(*, *) "NUMBER:", TRIM(short_number)

    READ(short_number, *) concat

END FUNCTION concat
