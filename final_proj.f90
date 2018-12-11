         !Danny Diaz
        !dd32387
        !12-9-18
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

       
        module prime_numbers

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
        
        integer, dimension(10000)  function successive_prime_nums_array(n)
                integer :: n, i, ierror
                integer ::  counter = 1, pot_prim_num = 3
                !integer, dimension(:), allocatable :: array 
                logical :: is_prime 
                integer, dimension(:), allocatable :: prime_array
                
               
                allocate(prime_array(n), stat=ierror)
                if (ierror /= 0) stop "error prime_array"


                do while (counter <= n)
                        is_prime = prime_test_function(pot_prim_num)
                        if (is_prime == .false.) then
                                pot_prim_num = pot_prim_num + 1

                        else if (is_prime) then
                                print *, pot_prim_num
                                prime_array(counter) =  pot_prim_num
                                pot_prim_num = pot_prim_num + 1
                                counter = counter + 1
                        end if
                end do
                print *, size(prime_array) 
        !        print '(15i8,2x)',prime_array
                successive_prime_nums_array = prime_array
        end function 


        end module prime_numbers

        program print_prime_numbers
                use prime_numbers
                implicit none
               
                integer :: input, inp_error
                integer,dimension(:), allocatable :: prime_nums_array
                
                print *, "How many successive prime numbers?"
                read *, input
                
                allocate(prime_nums_array(input), stat=inp_error)
                if (inp_error /= 0) stop "error prime_nums_array"

                prime_nums_array = successive_prime_nums_array(input)

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

