PROGRAM main

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), PARAMETER :: ex_file = "inputs/day_05_example.txt"
    CHARACTER(LEN=*), PARAMETER :: in_file = "inputs/day_05_inputs.txt"
    CHARACTER(LEN=32)           :: file
    LOGICAL                     :: test

    INTEGER, DIMENSION(:, :), ALLOCATABLE :: page_ordering_rules
    INTEGER, DIMENSION(:, :), ALLOCATABLE :: pages_to_produce
    INTEGER, DIMENSION(:, :), ALLOCATABLE :: correct_pages
    INTEGER, DIMENSION(:, :), ALLOCATABLE :: incorrect_pages

    INTERFACE
        SUBROUTINE read_safety_protocol(filename, page_ordering_rules, pages_to_produce)
            CHARACTER(LEN=*), INTENT(IN)                         :: filename
            INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: page_ordering_rules
            INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: pages_to_produce
        END SUBROUTINE read_safety_protocol

        SUBROUTINE check_update(pages_to_produce, page_ordering_rules, correct_pages, incorrect_pages)
            INTEGER, DIMENSION(:, :), INTENT(IN)                 :: pages_to_produce
            INTEGER, DIMENSION(:, :), INTENT(IN)                 :: page_ordering_rules
            INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: correct_pages
            INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: incorrect_pages
        END SUBROUTINE check_update

        SUBROUTINE calc_middle_sum(pages_to_produce)
            INTEGER, DIMENSION(:, :), INTENT(IN) :: pages_to_produce
        END SUBROUTINE calc_middle_sum
    END INTERFACE
    !=============================================================================================!
    
    test = .FALSE.
    IF(test)       file = ex_file
    IF(.NOT. test) file = in_file

    CALL read_safety_protocol(TRIM(file), page_ordering_rules, pages_to_produce)
    CALL check_update(pages_to_produce, page_ordering_rules, correct_pages, incorrect_pages)
    CALL calc_middle_sum(correct_pages)

    IF(ALLOCATED(page_ordering_rules)) DEALLOCATE(page_ordering_rules)
    IF(ALLOCATED(pages_to_produce)) DEALLOCATE(pages_to_produce)
    IF(ALLOCATED(correct_pages)) DEALLOCATE(correct_pages)
    IF(ALLOCATED(incorrect_pages)) DEALLOCATE(incorrect_pages)

END PROGRAM main


SUBROUTINE read_safety_protocol(filename, page_ordering_rules, pages_to_produce)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN)                         :: filename
    INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: page_ordering_rules
    INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: pages_to_produce

    INTEGER   :: f_unit
    INTEGER   :: io_stat
    INTEGER   :: num_rules
    INTEGER   :: num_updates
    INTEGER   :: max_pages
    INTEGER   :: num_pages
    INTEGER   :: i, j
    CHARACTER :: chr
    CHARACTER(LEN=256) :: line
    !=============================================================================================!

    OPEN(file=filename, action="read", status="old", newunit=f_unit)
    
    !---------------------------------------------------------- Determine size of input data -----!
    num_rules = 0
    DO
        READ(f_unit, "(A)") chr
        IF(chr == "") EXIT
        num_rules = num_rules + 1
    ENDDO

    ALLOCATE(page_ordering_rules(num_rules, 2))
    
    num_updates = 0
    max_pages   = 0
    DO
        READ(f_unit, "(A)", iostat=io_stat) line
        IF(io_stat /= 0) EXIT
        
        num_pages = 0
        DO i = 1, LEN_TRIM(line)
            chr = line(i:i)
            IF(chr == ",") num_pages = num_pages + 1
        ENDDO
        num_pages = num_pages + 1
        max_pages = MAX(num_pages, max_pages)
        
        num_updates = num_updates + 1
    ENDDO
    
    ! Allocate with max_pages + 1 to store number of pages per update...
    ALLOCATE(pages_to_produce(num_updates, max_pages + 1))

    WRITE(*, *) "NUMBER OF UPDATES:", num_updates
    WRITE(*, *) "NUMBER OF PAGES  :", max_pages
    !---------------------------------------------------------- Determine size of input data -----!

    REWIND(f_unit)
    
    !------------------------------------------------------------------ Read the actual data -----!
    DO i = 1, num_rules
        READ(f_unit, "(I2, A1, I2)") page_ordering_rules(i, 1), chr, page_ordering_rules(i, 2)
    ENDDO

    READ(f_unit, "(A)") chr

    DO i = 1, num_updates
        READ(f_unit, "(A)") line

        num_pages = 0
        DO j = 1, LEN_TRIM(line)
            chr = line(j:j)
            IF(chr == ",") num_pages = num_pages + 1
        ENDDO
        num_pages = num_pages + 1

        pages_to_produce(i, 1) = num_pages
        READ(line, *) pages_to_produce(i, 2:num_pages + 1)
    ENDDO
    !------------------------------------------------------------------ Read the actual data -----!
    
    CLOSE(f_unit)

END SUBROUTINE read_safety_protocol


SUBROUTINE check_update(pages_to_produce, page_ordering_rules, correct_pages, incorrect_pages)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, DIMENSION(:, :), INTENT(IN)                 :: pages_to_produce
    INTEGER, DIMENSION(:, :), INTENT(IN)                 :: page_ordering_rules
    INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: correct_pages
    INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: incorrect_pages
    
    LOGICAL, DIMENSION(:), ALLOCATABLE :: is_correct
    INTEGER, DIMENSION(:), ALLOCATABLE :: pages
    INTEGER :: i, j, k
    INTEGER :: n1, n2
    INTEGER :: index_1, index_2
    INTEGER :: num_pages
    !=============================================================================================!
    
    ALLOCATE(is_correct(SIZE(pages_to_produce, 1)))
    
    is_correct(:) = .TRUE.

    DO i = 1, SIZE(pages_to_produce, 1)
        num_pages = pages_to_produce(i, 1)
        ALLOCATE(pages(num_pages))
        
        pages(:) = pages_to_produce(i, 2:num_pages + 1)
        
        DO j = 1, SIZE(page_ordering_rules, 1)
            n1 = page_ordering_rules(j, 1)
            n2 = page_ordering_rules(j, 2)
            
            index_1 = -1
            index_2 = -1

            DO k = 1, SIZE(pages)
                IF(pages(k) == n1) index_1 = k
                IF(pages(k) == n2) index_2 = k
            ENDDO
            
            IF(index_1 /= -1 .AND. index_2 /= -1) is_correct(i) = is_correct(i) .AND. (index_1 < index_2)
        ENDDO

        DEALLOCATE(pages)
    ENDDO

    ALLOCATE(correct_pages(COUNT(is_correct), SIZE(pages_to_produce, 2)))
    
    j = 1
    DO i = 1, SIZE(is_correct)
        num_pages = pages_to_produce(i, 1)
        IF(is_correct(i)) THEN 
            correct_pages(j, 1)  = num_pages
            correct_pages(j, 2:num_pages + 1) = pages_to_produce(i, 2:num_pages + 1)
            j = j + 1
        ENDIF
    ENDDO

    DEALLOCATE(is_correct)

END SUBROUTINE check_update


SUBROUTINE calc_middle_sum(pages_to_produce)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, DIMENSION(:, :), INTENT(IN) :: pages_to_produce

    INTEGER :: middle_sum
    INTEGER :: i
    INTEGER :: num_pages
    !=============================================================================================!

    middle_sum = 0
    DO i = 1, SIZE(pages_to_produce, 1)
        num_pages = pages_to_produce(i, 1)
        middle_sum = middle_sum + pages_to_produce(i, num_pages / 2 + 2)
    ENDDO

    WRITE(*, *) "SUM OF MIDDLE PAGES:", middle_sum

END SUBROUTINE calc_middle_sum
