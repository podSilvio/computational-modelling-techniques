c File laplace.f
       subroutine laplace(a, b)
       double precision a(50 + 2, 50 + 2)
       double precision b(50 + 2, 50 + 2)
        
       integer :: n = 50
        
cf2py  intent(in) :: a
cf2py  intent(out) :: a
cf2py  intent(hide) :: n

       DO i=0, 51
            do j=0, 51
                IF ((i==0).or.(i==(n+1)).or.(j==0).or.(j==(n+1))) THEN
                    b(i,j) = a(i,j)
                ELSE
                    b(i,j) = (a(i,j-1)+a(i-1,j)+a(i,j+1)+a(i+1,j))/4
                END IF
            END DO
       END DO
       end
