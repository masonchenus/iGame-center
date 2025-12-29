! Fortran physics simulation for game development
! Advanced physics calculations using modern Fortran features
module physics_simulation
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    
    ! Physics constants
    real(dp), parameter :: PI = acos(-1.0_dp)
    real(dp), parameter :: GRAVITY = 9.81_dp
    real(dp), parameter :: AIR_RESISTANCE = 0.1_dp
    
    ! Physics state type
    type :: physics_state
        real(dp) :: position(3)    ! x, y, z coordinates
        real(dp) :: velocity(3)    ! vx, vy, vz velocities
        real(dp) :: acceleration(3) ! ax, ay, az accelerations
        real(dp) :: mass           ! Object mass
        real(dp) :: radius         ! Collision radius
        logical :: on_ground       ! Ground contact flag
    end type physics_state
    
    ! Collision result type
    type :: collision_result
        logical :: collided
        real(dp) :: normal(3)
        real(dp) :: penetration_depth
        real(dp) :: impact_velocity
    end type collision_result
    
    ! Public interface
    public :: physics_state, collision_result
    public :: update_physics, detect_collision, resolve_collision
    public :: apply_force, calculate_gravity, apply_air_resistance
    public :: create_sphere, create_box
    
contains
    
    ! Create a spherical physics object
    function create_sphere(x, y, z, radius, mass) result(obj)
        real(dp), intent(in) :: x, y, z, radius, mass
        type(physics_state) :: obj
        
        obj%position = [x, y, z]
        obj%velocity = [0.0_dp, 0.0_dp, 0.0_dp]
        obj%acceleration = [0.0_dp, 0.0_dp, 0.0_dp]
        obj%mass = mass
        obj%radius = radius
        obj%on_ground = .false.
    end function create_sphere
    
    ! Create a box-shaped physics object
    function create_box(x, y, z, width, height, depth, mass) result(obj)
        real(dp), intent(in) :: x, y, z, width, height, depth, mass
        type(physics_state) :: obj
        real(dp) :: max_dimension
        
        max_dimension = max(width, height, depth)
        obj%position = [x, y, z]
        obj%velocity = [0.0_dp, 0.0_dp, 0.0_dp]
        obj%acceleration = [0.0_dp, 0.0_dp, 0.0_dp]
        obj%mass = mass
        obj%radius = max_dimension / 2.0_dp
        obj%on_ground = .false.
    end function create_box
    
    ! Calculate gravitational force
    function calculate_gravity(mass) result(force)
        real(dp), intent(in) :: mass
        real(dp) :: force(3)
        
        force = [0.0_dp, -mass * GRAVITY, 0.0_dp]
    end function calculate_gravity
    
    ! Apply air resistance
    subroutine apply_air_resistance(obj)
        type(physics_state), intent(inout) :: obj
        real(dp) :: speed, drag_force(3)
        
        speed = sqrt(sum(obj%velocity**2))
        if (speed > 0.0_dp) then
            drag_force = -AIR_RESISTANCE * speed * obj%velocity / speed
            obj%acceleration = obj%acceleration + drag_force / obj%mass
        end if
    end subroutine apply_air_resistance
    
    ! Apply force to object
    subroutine apply_force(obj, force)
        type(physics_state), intent(inout) :: obj
        real(dp), intent(in) :: force(3)
        
        obj%acceleration = obj%acceleration + force / obj%mass
    end subroutine apply_force
    
    ! Update physics state
    subroutine update_physics(obj, dt, ground_y)
        type(physics_state), intent(inout) :: obj
        real(dp), intent(in) :: dt
        real(dp), intent(in), optional :: ground_y
        real(dp) :: ground_level
        real(dp) :: gravity_force(3)
        
        ! Set ground level
        if (present(ground_y)) then
            ground_level = ground_y
        else
            ground_level = 0.0_dp
        end if
        
        ! Apply gravity
        gravity_force = calculate_gravity(obj%mass)
        call apply_force(obj, gravity_force)
        
        ! Apply air resistance
        call apply_air_resistance(obj)
        
        ! Integrate velocity and position using Verlet integration
        obj%velocity = obj%velocity + obj%acceleration * dt
        obj%position = obj%position + obj%velocity * dt + 0.5_dp * obj%acceleration * dt**2
        
        ! Ground collision
        obj%on_ground = .false.
        if (obj%position(2) - obj%radius < ground_level) then
            obj%position(2) = ground_level + obj%radius
            if (obj%velocity(2) < 0.0_dp) then
                obj%velocity(2) = -obj%velocity(2) * 0.8_dp ! Bounce with energy loss
                obj%on_ground = .true.
            end if
        end if
        
        ! Reset acceleration
        obj%acceleration = [0.0_dp, 0.0_dp, 0.0_dp]
    end subroutine update_physics
    
    ! Detect collision between two objects
    function detect_collision(obj1, obj2) result(collision)
        type(physics_state), intent(in) :: obj1, obj2
        type(collision_result) :: collision
        real(dp) :: distance, min_distance
        
        ! Calculate distance between centers
        distance = sqrt(sum((obj1%position - obj2%position)**2))
        min_distance = obj1%radius + obj2%radius
        
        ! Check collision
        if (distance < min_distance) then
            collision%collided = .true.
            collision%penetration_depth = min_distance - distance
            
            ! Calculate collision normal
            if (distance > 0.0_dp) then
                collision%normal = (obj2%position - obj1%position) / distance
            else
                collision%normal = [1.0_dp, 0.0_dp, 0.0_dp]
            end if
            
            ! Calculate relative velocity along normal
            collision%impact_velocity = dot_product(obj1%velocity - obj2%velocity, collision%normal)
        else
            collision%collided = .false.
            collision%normal = [0.0_dp, 0.0_dp, 0.0_dp]
            collision%penetration_depth = 0.0_dp
            collision%impact_velocity = 0.0_dp
        end if
    end function detect_collision
    
    ! Resolve collision between two objects
    subroutine resolve_collision(obj1, obj2, collision)
        type(physics_state), intent(inout) :: obj1, obj2
        type(collision_result), intent(in) :: collision
        real(dp) :: total_mass, impulse, separation_force(3)
        
        if (.not. collision%collided) return
        
        ! Calculate impulse for elastic collision
        total_mass = obj1%mass + obj2%mass
        if (total_mass > 0.0_dp) then
            impulse = (1.0_dp + 0.8_dp) * collision%impact_velocity / total_mass
            
            ! Apply impulse
            obj1%velocity = obj1%velocity - impulse * obj2%mass * collision%normal
            obj2%velocity = obj2%velocity + impulse * obj1%mass * collision%normal
        end if
        
        ! Separate objects to avoid penetration
        separation_force = collision%normal * collision%penetration_depth * 0.5_dp
        obj1%position = obj1%position - separation_force
        obj2%position = obj2%position + separation_force
    end subroutine resolve_collision
    
    ! Ray-sphere intersection test
    function ray_sphere_intersection(ray_origin, ray_direction, sphere_center, sphere_radius) result(t)
        real(dp), intent(in) :: ray_origin(3), ray_direction(3)
        real(dp), intent(in) :: sphere_center(3), sphere_radius
        real(dp) :: t
        real(dp) :: oc(3), a, b, c, discriminant
        
        oc = ray_origin - sphere_center
        a = dot_product(ray_direction, ray_direction)
        b = 2.0_dp * dot_product(oc, ray_direction)
        c = dot_product(oc, oc) - sphere_radius**2
        
        discriminant = b**2 - 4.0_dp*a*c
        
        if (discriminant < 0.0_dp) then
            t = -1.0_dp ! No intersection
        else
            t = (-b - sqrt(discriminant)) / (2.0_dp * a)
            if (t < 0.0_dp) then
                t = (-b + sqrt(discriminant)) / (2.0_dp * a)
            end if
        end if
    end function ray_sphere_intersection
    
    ! Calculate bounding box from object
    subroutine calculate_bounding_box(obj, min_corner, max_corner)
        type(physics_state), intent(in) :: obj
        real(dp), intent(out) :: min_corner(3), max_corner(3)
        
        min_corner = obj%position - obj%radius
        max_corner = obj%position + obj%radius
    end subroutine calculate_bounding_box
    
end module physics_simulation

! Demo program
program physics_demo
    use physics_simulation
    implicit none
    
    type(physics_state) :: ball1, ball2, ball3
    type(collision_result) :: collision
    real(dp) :: dt, ground_y
    integer :: i
    
    ! Initialize ground level
    ground_y = 0.0_dp
    
    ! Create physics objects
    ball1 = create_sphere(0.0_dp, 10.0_dp, 0.0_dp, 1.0_dp, 1.0_dp)
    ball2 = create_sphere(3.0_dp, 15.0_dp, 0.0_dp, 1.5_dp, 2.0_dp)
    ball3 = create_box(-2.0_dp, 20.0_dp, 0.0_dp, 2.0_dp, 2.0_dp, 2.0_dp, 1.5_dp)
    
    ! Give initial velocities
    ball1%velocity = [2.0_dp, 0.0_dp, 0.0_dp]
    ball2%velocity = [-1.0_dp, 0.0_dp, 0.0_dp]
    ball3%velocity = [0.5_dp, 0.0_dp, 0.0_dp]
    
    print *, '=== Physics Simulation Demo ==='
    print *, 'Time  Ball1 Position    Ball2 Position    Ball3 Position'
    
    ! Simulate physics
    dt = 0.016_dp ! 60 FPS
    
    do i = 1, 300 ! 5 seconds at 60 FPS
        ! Update physics
        call update_physics(ball1, dt, ground_y)
        call update_physics(ball2, dt, ground_y)
        call update_physics(ball3, dt, ground_y)
        
        ! Check collisions
        collision = detect_collision(ball1, ball2)
        if (collision%collided) then
            call resolve_collision(ball1, ball2, collision)
        end if
        
        collision = detect_collision(ball1, ball3)
        if (collision%collided) then
            call resolve_collision(ball1, ball3, collision)
        end if
        
        collision = detect_collision(ball2, ball3)
        if (collision%collided) then
            call resolve_collision(ball2, ball3, collision)
        end if
        
        ! Print position every 30 frames
        if (mod(i, 30) == 0) then
            print '(F6.2, 3(2X, 3(F6.2, 1X)))', &
                real(i) * dt, &
                ball1%position, &
                ball2%position, &
                ball3%position
        end if
    end do
    
    print *, 'Physics simulation completed!'
end program physics_demo
