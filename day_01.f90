PROGRAM main

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, DIMENSION(:), ALLOCATABLE :: left_list
    INTEGER, DIMENSION(:), ALLOCATABLE :: right_list
    INTEGER                            :: input_size
    INTEGER                            :: distance
    INTEGER                            :: similiarity_score
    CHARACTER(LEN=*), PARAMETER        :: example_file = "./inputs/day_01_example.txt"
    CHARACTER(LEN=*), PARAMETER        :: input_file   = "./inputs/day_01_inputs.txt"
    CHARACTER(LEN=50)                  :: file
    LOGICAL                            :: test
    !=============================================================================================!
    
    WRITE(*, *) ""
    WRITE(*, *) "=========================="
    WRITE(*, *) "===== ADVENT OF CODE ====="
    WRITE(*, *) "=========================="
    WRITE(*, *) ""
    WRITE(*, *) "--- DAY 1 - Part I"
    WRITE(*, *) ""
    
    test = .FALSE.

    IF(test) THEN
        file = example_file
    ELSE
        file = input_file
    ENDIF

    CALL get_input_size(TRIM(file), input_size)

    WRITE(*, "(A, I0)") " SIZE: ", input_size
    ALLOCATE(left_list(input_size))
    ALLOCATE(right_list(input_size))

    CALL get_inputs(TRIM(file), input_size, left_list, right_list)

!    WRITE(*, *) left_list(:)
!    WRITE(*, *) right_list(:)

    CALL find_total_distance(input_size, left_list, right_list, distance)

    WRITE(*, "(A, I0)") " TOTAL DISTANCE: ", distance

    WRITE(*, *) ""
    WRITE(*, *) "--- DAY 1 - Part II"
    WRITE(*, *) ""

    CALL calculate_similiarity_score(input_size, left_list, right_list, similiarity_score)

    WRITE(*, "(A, I0)") " SIMILIARITY SCORE: ", similiarity_score
    WRITE(*, *) ""
    WRITE(*, *) "--- END DAY 1 ---"
    WRITE(*, *) ""

END PROGRAM main


SUBROUTINE get_input_size(filename, input_size)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER, INTENT(OUT)         :: input_size

    INTEGER            :: f_unit
    INTEGER            :: io_stat
    CHARACTER(len=100) :: line
    !=============================================================================================!

    input_size = 0
    OPEN(action="read", file=filename, newunit=f_unit)

    DO
        READ(f_unit, "(A)", iostat=io_stat) line
        IF(io_stat /= 0) EXIT
        input_size = input_size + 1
    ENDDO

    CLOSE(f_unit)

END SUBROUTINE get_input_size


SUBROUTINE get_inputs(filename, input_size, left_list, right_list)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN)                  :: filename
    INTEGER, INTENT(IN)                           :: input_size
    INTEGER, DIMENSION(input_size), INTENT(INOUT) :: left_list
    INTEGER, DIMENSION(input_size), INTENT(INOUT) :: right_list
    
    INTEGER :: f_unit
    INTEGER :: index
    INTEGER :: left
    INTEGER :: right
    !=============================================================================================!

    OPEN(file=filename, action="read", newunit=f_unit)
    
    DO index = 1, input_size
        READ(f_unit, *) left, right
        left_list(index)  = left
        right_list(index) = right
    ENDDO

    CLOSE(f_unit)

END SUBROUTINE get_inputs


SUBROUTINE find_total_distance(list_size, left_list, right_list, distance)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, INTENT(IN)                       :: list_size
    INTEGER, DIMENSION(list_size), INTENT(IN) :: left_list
    INTEGER, DIMENSION(list_size), INTENT(IN) :: right_list
    INTEGER, INTENT(OUT)                      :: distance

    LOGICAL, DIMENSION(list_size) :: left_mask
    LOGICAL, DIMENSION(list_size) :: right_mask
    INTEGER               :: min_left
    INTEGER, DIMENSION(1) :: min_left_index
    INTEGER               :: min_right
    INTEGER, DIMENSION(1) :: min_right_index
    !=============================================================================================!
    
    left_mask(:)  = .TRUE.
    right_mask(:) = .TRUE.
    
    distance = 0

    DO WHILE(ANY(left_mask))
        min_left       = MINVAL(left_list, mask=left_mask)
        min_left_index = MINLOC(left_list, mask=left_mask)

        min_right       = MINVAL(right_list, mask=right_mask)
        min_right_index = MINLOC(right_list, mask=right_mask)

        distance = distance + ABS(min_left - min_right)

        left_mask(min_left_index(1)) = .FALSE.
        right_mask(min_right_index(1)) = .FALSE.
    ENDDO

END SUBROUTINE find_total_distance


SUBROUTINE calculate_similiarity_score(list_size, left_list, right_list, similiarity_score)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, INTENT(IN)                       :: list_size
    INTEGER, DIMENSION(list_size), INTENT(IN) :: left_list
    INTEGER, DIMENSION(list_size), INTENT(IN) :: right_list
    INTEGER, INTENT(OUT)                      :: similiarity_score
    
    INTEGER :: left
    INTEGER :: right_multiplier
    INTEGER :: left_index
    INTEGER :: right_index
    !=============================================================================================!
    
    similiarity_score = 0

    DO left_index = 1, list_size
        left = left_list(left_index)
        right_multiplier = 0

        DO right_index = 1, list_size
            IF(left == right_list(right_index)) right_multiplier = right_multiplier + 1
        ENDDO

        similiarity_score = similiarity_score + left * right_multiplier
    ENDDO

END SUBROUTINE calculate_similiarity_score
