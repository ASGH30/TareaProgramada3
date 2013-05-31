(*
[Lenguajes de Programacion]
[Tarea Programada #3]
[Lector de Fechas]
[Estudiante: Adrian Siles]
[Carnet: 201136841]
*)

fun leer (File : string) = (*funcion que recibe el nombre del archivo he intenta leerlo *)
    let val file2 = TextIO.openIn File
	fun ciclo file2 =
	    case TextIO.inputLine file2 of
		SOME line => line :: ciclo file2
	      | NONE      => []
    in
	ciclo file2
    end
fun explode_lines (out: char list, In: string list)= (*funcion que recibe el contenido del archivo y los separa en chars*)
    if null In
    then out
    else
	explode_lines(out@explode(hd(In)),tl In)

fun EsMayor (y, x)= (*auxiliar de la funcion recorte*)
    let val extra = Int.toString(y);
	in
	    if x=1
	    then
		if extra <"8"
		then true
		else false
	    else
		if extra<"9"
		then true
		else false
    end
fun aux (out: char list list, In: char list, Num: int)= (*auxiliar de la funcion recorte*)
    let fun pasar (I2: char list, num:int)=
	    if num=0
	    then []
	    else (hd I2):: pasar(tl I2,num-1)
    in
	let val temp = pasar(In,Num);
	in
	    out@[temp]
	end
    end
	
fun recorte (out: char list list, In: char list)= (*funcion que saca las fechas validas*)
    if null In
    then out
    else 
	if EsMayor((length In),1)
	then recorte(out,tl In)
	else
	    if (Char.isDigit(hd In)) andalso (Char.isDigit(hd(tl(In)))) andalso (hd(tl(tl(In))) = #"/") andalso (Char.isDigit(hd(tl(tl(tl(In)))))) andalso (Char.isDigit(hd(tl(tl(tl(tl(In))))))) andalso ((hd(tl(tl(tl(tl(tl(In))))))) = #"/") andalso (Char.isDigit(hd(tl(tl(tl(tl(tl(tl(In))))))))) andalso (Char.isDigit(hd(tl(tl(tl(tl(tl(tl(tl(In))))))))))
	    then  
		if EsMayor((length In),2)
		then 
		    if (Char.isDigit(hd(tl(tl(tl(tl(tl(tl(tl(tl(In))))))))))) andalso (Char.isDigit(hd(tl(tl(tl(tl(tl(tl(tl(tl(tl(In))))))))))))
		    then recorte(aux(out,In,10),tl(tl(tl(tl(tl(tl(tl(tl(tl(tl(In)))))))))))
		    else recorte(aux(out,In,8),tl(tl(tl(tl(tl(tl(tl(tl(tl(tl(In)))))))))))
		else recorte(aux(out,In,8),tl(tl(tl(tl(tl(tl(tl(tl(In)))))))))
	    else recorte(out,tl(tl(tl(tl(tl(tl(tl(tl(In)))))))))

fun listar (listaO:char list, num_extraido:string, Ltemp: int list, retorno: int list list, contPos: int)= (*funcion que recibe la lista de chars de fechas validas, y las pasa a un (int*int*int) list si son fechas validas*)
    if null listaO
    then retorno
    else
	if  contPos<10
	then
	    if Char.isDigit(hd listaO)
	    then
		if contPos=0 orelse contPos=3 orelse contPos=6 (* un 1er numero necesita que se le encadene el siguiente*)
		then listar(tl listaO, str(hd listaO), Ltemp, retorno, contPos+1 )
		else
		    if contPos=1 orelse contPos=4 orelse contPos=9 (*si es un numero y pertenece a la segunda posicion se debe encadenar al numero anterior*)
		    then listar(tl listaO,"", Option.getOpt(Int.fromString(num_extraido^str( hd listaO)),0)::Ltemp, retorno,contPos+1 )
		    else
			if contPos=7 orelse contPos=8
			then listar(tl listaO, num_extraido^str(hd listaO), Ltemp, retorno, contPos+1 )		    
			else listar(tl listaO,"",[],retorno,0) (*hay un error xq un numero no puede estar en una posicion diferente de estas, se reinicia temp y contador*)
	    else
		if (hd listaO) = #"/"
		then
		    if contPos=2 orelse contPos=5 (*unicas posiciones del separador de fecha*)
		    then listar(tl listaO, "", Ltemp, retorno, contPos+1 )
		    else listar(tl listaO, "", [], retorno, 0) (*hay un error xq un separador no puede estar en una posicion diferente de estas, se reinicia temp y contador*)
		else
		    listar(tl listaO, "", [], retorno, 0)
	else
	    listar(tl listaO, "", [], rev(Ltemp)::retorno, 0) (*el contador esta lleno por lo que se debe pasar la fecha reversada a la lista (debido a que se almaceno con encadenamientos en la cola), se debe reiniciar temp y el contador*)

fun LpasaT (listaO: int list list, retorno: (int * int * int) list )=
    if null listaO
    then retorno
    else 
	let fun LTAux(l1,l2,l3,LO)=
		if null LO
		then (l1,l2,l3)
		else
		    if length LO = 1
		    then LTAux(l1,l2,hd LO, tl LO)
		    else 
			if length LO = 2
			then LTAux (l1, hd LO, l3, tl LO)
			else LTAux (hd LO, l2, l3, tl LO)
	in
	    let val temp=LTAux(0,0,0,hd listaO)
	    in
		LpasaT(tl listaO, temp::retorno)
	    end
	end

val StrMeses= ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre"];

(*Funcion que identifca los meses*)
fun hacerStrM (trio2: int, Meses: string list) =
  if null Meses
  then
      "xMes"
  else
      if trio2=length(Meses)
      then
	  hd(Meses)
      else hacerStrM(trio2, tl Meses);

fun hacerStr (trio: (int * int * int) list , lista: string list ) = 
    if null trio
    then lista
    else 
	hacerStr(tl trio, (Int.toString(#1(hd trio))) ^ " de "  ^ hacerStrM(#2(hd trio), StrMeses)^" del " ^ Int.toString(#3(hd trio))::lista);

fun imprimir (listaO: (int*int*int) list) =
    if null listaO
    then ["Fin"]
    else
	hacerStr(listaO, [])

fun fechas (listaO: (int * int * int) list )= 
    if null listaO
    then ["Disculpe, su archivo no contiene fechas validas"]
    else 
	imprimir(listaO)

fun empezar (X: string)=
   fechas(rev(tl(rev(LpasaT(listar(explode_lines([],leer(X)),"",[],[],0),[(0,0,0)])))))

fun main (archivo: string)=
    empezar(archivo);
