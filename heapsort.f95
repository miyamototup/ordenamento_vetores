program ordenamento_heapsort
use ordenamento
  implicit none
  do j=1,5
    allocate(v(num(j)))
    call random_seed
    call random_number(v)
    !write(*,*) "Exemplo de vetor não ordenado:"
    !write(*,*) v
    !write(*,*)
    call heapsort(v)
    !write(*,*) "O mesmo vetor ordenado"
    !write(*,*) v
    print *, "Ordenamento de N elementos sendo, N =",(num(j))
    print *, "Tempo de execução em segundos da subrotina heapsort = ", t1-t0
    deallocate(v)
  end do
end program ordenamento_heapsort 

