PROGRAM main

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER                     :: index
    INTEGER                     :: length
    INTEGER                     :: number_1
    INTEGER                     :: number_2
    INTEGER, PARAMETER          :: max_length = 8  ! 3 + 1 + 3 + 1 == 123,123)
    CHARACTER(LEN=10)           :: numbers
    CHARACTER(LEN=*), PARAMETER :: test_line = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    CHARACTER, DIMENSION(:), ALLOCATABLE :: corrupted_memory
    INTEGER                              :: memory_length

    INTERFACE
        SUBROUTINE read_corrupted_memory(filename, content, content_length)
            CHARACTER(LEN=*), INTENT(IN)                        :: filename
            CHARACTER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: content
            INTEGER, INTENT(OUT)                                :: content_length
        END SUBROUTINE read_corrupted_memory

        INTEGER FUNCTION mul_result(content, content_length)
            INTEGER, INTENT(IN)                              :: content_length
            CHARACTER, DIMENSION(content_length), INTENT(IN) :: content
        END FUNCTION mul_result
    END INTERFACE
    !=============================================================================================!
    
    WRITE(*, *) "Test result:", mul_result(test_line, LEN(test_line))

    CALL read_corrupted_memory("inputs/day_03_inputs.txt", corrupted_memory, memory_length)

    WRITE(*, *) "Mul result:", mul_result(corrupted_memory, memory_length)

    DEALLOCATE(corrupted_memory)

END PROGRAM main


SUBROUTINE read_corrupted_memory(filename, content, content_length)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN)                        :: filename
    CHARACTER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: content
    INTEGER, INTENT(OUT) :: content_length

    INTEGER :: f_unit
    INTEGER :: io_stat
    INTEGER :: i
    CHARACTER :: chr
    !=============================================================================================!
    
    OPEN(file=filename, action="read", status="old", newunit=f_unit)
    
    content_length = 0
    DO
        READ(f_unit, "(A)", iostat=io_stat, advance="no") chr
        !WRITE(*, *) chr
        IF(io_stat /= 0) EXIT
        content_length = content_length + 1
    ENDDO

    WRITE(*, *) "Length of content:", content_length

    ALLOCATE(content(content_length))

    REWIND(f_unit)
    
    DO i = 1, content_length
        READ(f_unit, "(A)", advance="no", iostat=io_stat) content(i)
        IF(io_stat /= 0) EXIT
    ENDDO

    !WRITE(*, *) content

    CLOSE(f_unit)

END SUBROUTINE read_corrupted_memory


INTEGER FUNCTION mul_result(content, content_length)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, INTENT(IN)                              :: content_length
    CHARACTER, DIMENSION(content_length), INTENT(IN) :: content

    INTEGER :: index
    INTEGER :: length
    INTEGER :: n1, n2
    INTEGER :: i
    CHARACTER, DIMENSION(10) :: numbers
    CHARACTER(LEN=4) :: pattern
    !=============================================================================================!
    
    mul_result = 0

    MUL_FINDER: DO index = 1, content_length - 4
        
        IF(    content(index    ) == "m" &
        &.AND. content(index + 1) == "u" &
        &.AND. content(index + 2) == "l" &
        &.AND. content(index + 3) == "(") THEN
            DO length = 0, 8
                IF(content(index + 4 + length) == ")") THEN
                    numbers = content(index + 4:index + 3 + length)
                    READ(numbers, *) n1, n2
                    mul_result = mul_result + n1 * n2
                    CYCLE MUL_FINDER
                ENDIF
            ENDDO
        ENDIF
    ENDDO MUL_FINDER

END FUNCTION mul_result
