program test

   IMPLICIT NONE

    REAL(8), PARAMETER :: e = 2.718281828
	INTEGER, PARAMETER :: cols = 20000
	INTEGER, PARAMETER :: levels = 80
	INTEGER, PARAMETER :: target_size_input = 1000000000 			! 1GB in bytes
	INTEGER, PARAMETER :: sm = 100 									! number of streaming multiprocessors
	INTEGER, PARAMETER :: threads_per_block_lw = 140
	INTEGER, PARAMETER :: threads_per_block_sw = 112
	INTEGER, PARAMETER :: target_size_working_set_data = 1000000 	! 1MB in bytes
	INTEGER, PARAMETER :: repetition = 1							! number of times kernel gets executed
	INTEGER ::	input_data_size, 			&
	&			working_set_data_size_lw, 	&
	&			working_set_data_size_sw, 	&
	&			num_thread_blocks_lw, 		&
	&			num_thread_blocks_sw
	INTEGER :: i,j,k,l,m,n
	INTEGER :: start_col, end_col, step_lw, step_sw

	REAL(8), DIMENSION(:,:,:), ALLOCATABLE :: input
	REAL(8), DIMENSION(:,:), ALLOCATABLE :: working_set_data_lw, working_set_data_sw

	! calcualate data values to match target sizes
	input_data_size = target_size_input/(cols*levels)
	working_set_data_size_lw = input_data_size
	working_set_data_size_sw = input_data_size

	! calculate parallelism
	num_thread_blocks_lw = 2048 / threads_per_block_lw
	num_thread_blocks_sw = 2048 / threads_per_block_sw
	start_col = cols / (sm * num_thread_blocks_lw)
	step_lw = cols/(sm*threads_per_block_lw)
	step_sw = cols/(sm*threads_per_block_sw)

	
	! allocate arrays
	allocate(input(input_data_size,levels,cols))
	allocate(working_set_data_lw(working_set_data_size_lw,step_lw))
	allocate(working_set_data_sw(working_set_data_size_sw,step_sw))

	!$OMP PARALLEL DO PRIVATE(working_set_data_lw)
	DO i=1,sm
		!$ACC ENTER DATA CREATE(working_set_data_lw)
		!$ACC PARALLEL DEFAULT (NONE) &
		!$ACC COPYIN(input,working_set_data_lw,step_lw,working_set_data_size_lw,num_thread_blocks_lw) &
		!$ACC PRIVATE (start_col,end_col)
		!$ACC LOOP GANG
		DO j=1,num_thread_blocks_lw
			start_col = ( (j-1)*step_lw ) + 1
			end_col = j * step_lw
			!$ACC LOOP VECTOR
			DO k=1,levels
				DO l=1,repetition
					working_set_data_lw(:,:) = input(:,k,start_col:end_col)
					DO m=1,working_set_data_size_lw
						DO n=1,step_lw
							working_set_data_lw(m,n) = e ** working_set_data_lw(m,n)
						END DO
					END DO
					input(:,k,start_col:end_col) = working_set_data_lw(:,:)
				END DO
			END DO
		END DO
		!$ACC END PARALLEL
		
		!$ACC EXIT DATA DELETE(working_set_data_lw)
	END DO
	!$OMP END PARALLEL
		



	print *,'hello World!'


end program test
