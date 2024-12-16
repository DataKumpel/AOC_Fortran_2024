MODULE signal_class
    TYPE signal
        CHARACTER :: freq
        INTEGER   :: pos_x
        INTEGER   :: pos_y
    END TYPE signal

CONTAINS
    SUBROUTINE scan_for_signals(map, signals)
        
        IMPLICIT NONE
        !=====================================================================================!
        CHARACTER, DIMENSION(:, :), INTENT(IN)                 :: map
        TYPE(signal), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: signals
            
        INTEGER :: i, j, k
        INTEGER :: num_signals
        !=====================================================================================!
            
        num_signals = 0
        
        DO i = 1, SIZE(map, 1)
            DO j = 1, SIZE(map, 2)
                IF(map(i, j) /= ".") num_signals = num_signals + 1
            ENDDO
        ENDDO
        
        ALLOCATE(signals(num_signals))
            
        k = 1
        DO i = 1, SIZE(map, 1)
            DO j = 1, SIZE(map, 2)
                IF(map(i, j) /= ".") THEN
                    signals(k)%freq  = map(i, j)
                    signals(k)%pos_x = j
                    signals(k)%pos_y = i
                    k = k + 1
                ENDIF
            ENDDO
        ENDDO
        
        !--- DEBUG
        DO k = 1, num_signals
            WRITE(*, *) signals(k)
        ENDDO
        
    END SUBROUTINE scan_for_signals

END MODULE signal_class


PROGRAM main
    
    USE signal_class

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), PARAMETER :: ex_file = "inputs/day_08_example.txt"
    CHARACTER(LEN=*), PARAMETER :: in_file = "inputs/day_08_inputs.txt"
    CHARACTER(LEN=32)           :: file
    LOGICAL                     :: test

    CHARACTER, DIMENSION(:, :), ALLOCATABLE :: map
    TYPE(signal), DIMENSION(:), ALLOCATABLE :: signals

    INTERFACE
        SUBROUTINE read_map(filename, map)
            CHARACTER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: map
            CHARACTER(LEN=*), INTENT(IN)                           :: filename
        END SUBROUTINE read_map
    END INTERFACE
    !=============================================================================================!

    test = .TRUE.
    IF(      test) file = ex_file
    IF(.NOT. test) file = in_file

    CALL read_map(TRIM(file), map)
    CALL scan_for_signals(map, signals)

    IF(ALLOCATED(map))     DEALLOCATE(map)
    IF(ALLOCATED(signals)) DEALLOCATE(signals)

END PROGRAM main


SUBROUTINE read_map(filename, map)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN)                           :: filename
    CHARACTER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: map

    INTEGER :: f_unit
    INTEGER :: io_stat
    INTEGER :: size_x, size_y
    INTEGER :: i, j
    CHARACTER(LEN=128) :: line
    CHARACTER          :: chr
    !=============================================================================================!

    OPEN(file=filename, action="read", status="old", newunit=f_unit)
    
    size_x = 0

    DO
        READ(f_unit, "(A)", iostat=io_stat, advance="no") chr
        IF(io_stat /= 0) EXIT
        size_x = size_x + 1
    ENDDO

    REWIND(f_unit)

    size_y = 0

    DO
        READ(f_unit, "(A)", iostat=io_stat) chr
        IF(io_stat /= 0) EXIT
        size_y = size_y + 1
    ENDDO

    REWIND(f_unit)

    ALLOCATE(map(size_y, size_x))

    DO i = 1, size_y
        READ(f_unit, "(A)") line

        DO j = 1, size_x
            map(i, j) = line(j:j)
        ENDDO

        WRITE(*, *) map(i, :)
    ENDDO

    CLOSE(f_unit)

END SUBROUTINE read_map
