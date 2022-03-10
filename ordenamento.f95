module ordenamento
  integer, parameter, dimension(5) :: num=(/20000, 40000, 80000, 160000, 320000/)
  double precision, dimension (:), allocatable :: v
  integer :: i, n, f, c, p, j
  double precision :: t0,t1,aux 

contains
 
subroutine heapsort(a)
  double precision, intent(in out) :: a(0:)
   call cpu_time(t0)
   n = size(a)
   do i = (n - 2) / 2, 0, -1
     call peneira(a, i, n);
   end do
   do f = n - 1, 1, -1
     aux = a(0)
     a(0) = a(f)
     a(f) = aux;
     call peneira(a, 0, f)
   end do
call cpu_time(t1)
end subroutine heapsort
 
subroutine peneira(a, i, f)
  double precision, intent(in out) :: a(0:)
  integer, intent(in) :: i, f
  p = i
  do while(p*2 + 1 < f)
    c = p * 2 + 1
     if (c + 1 < f) then
      if (a(c) < a(c+1)) c = c + 1
    end if
     if (a(p) < a(c)) then
      aux = a(c)
      a(c) = a (p)
      a(p) = aux
      p = c
    else
      return
    end if  
  end do      
 end subroutine peneira

end module ordenamento 

