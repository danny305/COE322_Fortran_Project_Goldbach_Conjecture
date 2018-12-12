         !Danny Diaz
        !dd32387
        !12-3-18
        !Final Project


        !*********Goldbach Conjecture**********

        !Every even number(starting with 4), is the sum of two primes
        ! p + q = 2n
        !For every prime number p, there exists a prime number q where:
        ! r = p + (p - q)
        
        !Find all prime numbers p, find the next prime number after p
        !let's this q. 

        !if p - (q - p) is also prime we'll call that r, and you found a
        !triple we're looking for. Do this for the first 10,000 prime
        !numbers

        !Allocate an array where you record all the p - q distances that
        !you found. Print some elementary statistics, for instance: what
        !is the average distance, do the distances increase or decrease
        !with p?

       
        module Goldbach_Conjecture

        implicit none
       
         
        contains
        logical function prime_test_function(num)
                integer :: i, num
                do i = 2, num -1
                        if (modulo(num,i) == 0) then
                                prime_test_function = .false.
                                exit
                        end if
        
                        if (i == num - 1) then
                                prime_test_function = .true.
                        end if
                end do
        end function
        
        subroutine prime_nums_array(n,prime_array)
                integer :: n, ierror
                integer ::  counter = 1, pot_prim_num = 3
                integer, dimension(n) :: prime_array 
                logical :: is_prime 

                do while (counter <= n)
                        is_prime = prime_test_function(pot_prim_num)
                        if (is_prime == .false.) then
                                pot_prim_num = pot_prim_num + 1

                        else if (is_prime) then
!                                print *, pot_prim_num
                                prime_array(counter) =  pot_prim_num
                                pot_prim_num = pot_prim_num + 1
                                counter = counter + 1
                        end if
                end do
                !print *, size(prime_array) 
                !print '(15i8,2x)',prime_array

        end subroutine prime_nums_array

        subroutine find_equi_dist_prime_triplets(n,prime_array,&
                                                equi_dist_array)

                integer :: n,r,p,q, j,k, dist1, dist2, size_array
                integer :: loop_counter = 1
                logical :: r_prime
                integer, dimension(n+100) :: prime_array,equi_dist_array
                
                call prime_nums_array(n+100, prime_array)

                size_array = size(prime_array)
!                print *, size_array

                print '(5a8,2x)', "count", "r","p", "q", "Dist"
                do j=1,n 
                        p = prime_array(j)
                        do k = j + 1, size(prime_array)
                                q = prime_array(k)
                                r = p -(q - p)
                                if (r < 0) then 
                !                        print *, "R is negative"
                                        exit
                                end if

                                r_prime = prime_test_function(r)
                                dist1 = q - p
                                dist2 = p - r
                                if (r_prime .AND. dist1 == dist2) then
                                        equi_dist_array(loop_counter)= &
                                                                   dist1
                                        print '(5i8,2x)', loop_counter,r,p,q, dist1
                                        loop_counter = loop_counter + 1
                                        exit
                                end if
                        end do    
                end do 
        end subroutine 

        subroutine statistical_analysis(n,equi_dist_array)
                integer :: i,n
                real :: MEAN, MEAN_1, VARIANCE, STD_DEV, n_float
                integer, dimension(n) :: equi_dist_array
                !print *, equi_dist_array
                

                !open(unit=1, file="conj_data1.txt", position='append')

                !write (1,'(4a10,2x)') "N","Mean", "Variance","Std_Dev"
                print "(4a10,2x)", "N", "MEAN", "VARIANCE", "STD_DEV"
                n_float = n

                MEAN = 0.0
                do i = 1, n
                        MEAN = MEAN + equi_dist_array(i) 
                end do
                MEAN = MEAN / n
                
                VARIANCE = 0.0 
                do i = 1,n
                        VARIANCE = VARIANCE + (equi_dist_array(i) - &
                        MEAN)**2
                end do
                VARIANCE = VARIANCE/(n - 1)
                STD_DEV = SQRT(VARIANCE)
 
                !write (1, '(4f10.2,x)') n_float, MEAN, VARIANCE, STD_DEV
                print '(4f10.2,x)', n_float, MEAN, VARIANCE, STD_DEV
                !close(1)
        end subroutine statistical_analysis
        end module Goldbach_Conjecture

        program main
                use Goldbach_Conjecture
                implicit none              
                
                integer :: n, ierror
                integer, dimension(:), allocatable :: &
                        array,triplet_array
                
                
                print *, "How many successive prime numbers?"
                read *, n

                allocate(array(n), stat=ierror)
                if (ierror /= 0) stop "error prime_array"

                
                allocate(triplet_array(n), stat=ierror)
                if (ierror /= 0) stop "error triplet_array"

!                call prime_nums_array(n,array)
                call find_equi_dist_prime_triplets(n, array,&
                                                  triplet_array)
                call statistical_analysis(n, triplet_array)

        end program




!        program Goldbach
!                use prime_numbers
!                implicit none
!
!                logical :: is_prime
!                integer :: input_int
!
!                print *, "Enter an integer to check if prime:"
!                read *,input_int
!
!
!                is_prime = prime_test_function(input_int)
!                
!                print *, "The integer: ", input_int, "is prime? ", & 
!                is_prime        
!
!        end program 

