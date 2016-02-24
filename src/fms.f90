!!
! fms.f90: A closed maze solver
!
! Created: Friday, February 5, 2016
! Author: Jessy Williams
! Contact: jessy@jessywilliams.com
!!

program maze_solve

    use fms_types
    use fms_msg
    use fms_solve
    implicit none

    character(len=R_BUFFER) :: arg
    character(len=R_BUFFER) :: path
    type(maze_t) :: maze

    ! Initialize map
    call get_command_argument(1, arg)  ! get any cli args
    path = get_path(arg)  ! get file path
    maze = parse(path)

    ! Solve map, present results
    call mapout(maze,solve(maze))

end program maze_solve
