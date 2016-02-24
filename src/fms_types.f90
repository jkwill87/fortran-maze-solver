!!
! fms_types.f90: user defined types for fms
!
! Created: Friday, February 5, 2016
! Author: Jessy Williams
! Contact: jessy@jessywilliams.com
!!

module fms_types
    
    implicit none
    
    !!
    ! path_t:
    !   a discrete coordinate which contains the positional information and 
    !   state of a cell within a maze
    !
    ! Elements:
    !   - y (integer) the row offset of the cell (starting from 1) 
    !   - x (integer) the column offset of the cell (starting from 1) 
    !   - s (character) an abreviation of state, the kind of character 
    !     represented within the maze
    !!

    type :: path_t
        integer   :: y = 0
        integer   :: x = 0
        character :: s = '?'
    end type path_t


    !!
    ! stack_t:
    !   contains elements required to store a stack of path_t types
    !
    ! Elements:
    !   - index (integer): an index pointing to the top of the stack
    !   - paths (path_t array): an array of path_t coordinates; to be 
    !     (re)sized as required 
    !!

    type :: stack_t
        integer                               :: index = 1
        type(path_t),dimension(:),allocatable :: paths
    end type stack_t


    !!
    ! maze_t:
    !   stores the paramaters of a maze and represents its state using a 2D 
    !   array
    !
    ! Elements:
    !   - grid (character array): a 2D array containing the maze values
    !   - startY(integer): the row offset for the maze's starting position 
    !   - startX(integer): the column offset for the maze's starting position 
    !!

    type :: maze_t
        character, allocatable :: grid(:,:)
        integer                :: cols   = 0
        integer                :: rows   = 0
        integer                :: startY = 0
        integer                :: startX = 0
    end type maze_t

contains


    !!
    ! init:
    !   initializes a stack based on the dimensions of a maze
    !
    ! Parameters:
    !   - stack (stack_t): the stack to initialize
    !   - rows (integer): the height of the maze
    !   - cols (integer): the width of the maze
    !
    ! Postconditions:
    !   - the grid element of the passed maze_t is allocated
    !!

    subroutine init(stack,rows,cols)
        type(stack_t),intent(inout) :: stack
        integer,intent(in)          :: rows
        integer,intent(in)          :: cols

        allocate(stack%paths(rows*cols))
    end subroutine init

    !!
    ! push:
    !   pushes a path_t coordinate to a stack
    !
    ! Parameters:
    !   - stack (stack_t): the stack to push to
    !   - path (path_t): the path to push
    !
    ! Postconditions:
    !   - the value of the passed path is pushed to the stack's path_t array
    !   - the stack's index is incremented by one
    !!

    subroutine push(stack,path)
        type(stack_t),intent(inout) :: stack
        type(path_t),intent(in)     :: path

        stack%paths(stack%index) = path
        stack%index = stack%index+1
    end subroutine push


    !!
    ! pop:
    !   pops a path_t coordinate off a stack
    !
    ! Parameters:
    !   - stack (stack_t): the stack to pop from
    !
    ! Postconditions:
    !   - the stack's index is decremented by one (if >0)
    !
    ! Return:
    !   - the value popped off the stack
    !!

    function pop(stack)
        type(stack_t),intent(inout) :: stack
        type(path_t)                :: pop

        if(stack%index > 0) stack%index = stack%index -1
        pop = stack%paths(stack%index)

    end function pop
    

    !!
    ! top:
    !   returns the value at the top of the stack
    !
    ! Parameters:
    !   - stack (stack_t): the stack to look into
    !
    ! Return:
    !   - the value popped off the stack
    !!

    pure function top(stack)
        type(stack_t),intent(in) :: stack
        type(path_t)             :: top

        top = stack%paths(stack%index -1)

    end function top


    !!
    ! destroy:
    !   returns the stack to its pre-initialized state
    !
    ! Parameters:
    !   - stack (stack_t): the stack to reset
    !
    ! Postcondition:
    ! - The stack's path_t array is deallocated
    ! - The stack's index is (re)set to 0
    !
    ! Return:
    !   - the value popped off the stack
    !!

    subroutine destroy(stack)
        type(stack_t),intent(inout) :: stack

        deallocate(stack%paths)
        stack%index = 0
    end subroutine destroy

end module fms_types