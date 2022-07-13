	program derivada_teste 
	implicit none
	real*8 x , Fx, deriv, dx, minha_funcao
	integer i
	open(unit=1,file='funcao.dat')
	open(unit=2,file='derivada.dat')

	!tamanho do intervalo
	dx=0.1d0
		do i= -2,2
			x=i/1000.0
			! chamando a rotina que vai calcular a derivada
			call derivada(deriv,dx,x)
  			write(1,*) x,minha_funcao(x)
			write(*,*) x,deriv
		end do
	
	end program

	!definindo a funcao

	function minha_funcao(x)
	real*8 minha_funcao,x
	
	minha_funcao = cos(exp(x**2+y**2))	

	return
	end function	


	subroutine derivada(deriv,dx,x)
	real*8 , intent(in) :: x, dx
	real*8 , intent(out) :: deriv
	real*8 minha_funcao
	
	!calculo da derivada
	deriv=(minha_funcao(x+dx) - minha_funcao(x-dx))/(2*dx)
	
	end subroutine

