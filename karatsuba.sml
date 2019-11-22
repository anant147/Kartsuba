(*
val append = int list * int -> int list

It is called by makeequal function.It append elements with value (0) w times in the beginning of the list.
*)
fun append([],w)=[]
| append(L,w)= if w=0 then L else 0::append(L,w-1);

(*
val removezero = int list -> int list

It is called by makeequal function.It is used to remove elements with value(0) in beginning of list.
*)
fun removezero([])=[]
| removezero([h])=[h]
|  removezero(h::t)= if h=0 then removezero(t) else (h::t);


(*
val makeequal = int list * int list -> int list * int list

It is called by add and subtract function respectively.It make use of removezero function to remove elements with 
value zero in beginning of list and then compare their lengths.After that , it make use of append function to make their length equal.
*)
fun makeequal([],[])=([],[])
| makeequal(L1,L2)=if length(removezero(L1))=length(removezero(L2)) then (removezero(L1),removezero(L2))
                   else if length(removezero(L1))>length(removezero(L2)) then (removezero(L1),append(removezero(L2), length(removezero(L1))-length(removezero(L2))))
                   else ( append(removezero(L1), length(removezero(L2))-length(removezero(L1))), removezero(L2) );


(*
val padder1 = int list * int -> int list

It is called by padder function.It appends element of integer value(0) w times in given input list.
*)
fun padder1(L,0)=L
| padder1(L,w)=0::padder1(L,w-1);


(*
val padder = int list * int -> int list

It is called by karatsuba function.It calls padder1 function with reverse of 
given list and integer value and it then reverse the output obtained by padder1 function. 
*)
fun padder([],w)=[]
| padder(L,w)=rev(padder1(rev(L),w));

(*
val spliting = int list * int -> int list * int list

It is called by karatsuba function.It is used to split the list.
*)
fun spliting([],w)=([],[])
| spliting(L,w)=(List.take(L,length(L)-w), List.drop(L,length(L)-w));

(*
val subtract1 = int list * int list * int -> int list

It is called by compare function.It computes absolute difference of two lists with borrow generated during traversing of lists.
*)
fun subtract1([],[],k)=[]
| subtract1(L1,L2,1)= if hd(L1)<(hd(L2)+1) then (9999+hd(L1)-hd(L2))::subtract1(tl(L1),tl(L2),1) else (hd(L1)-hd(L2)-1)::subtract1(tl(L1),tl(L2),0)
| subtract1(L1,L2,0)= if hd(L1)<hd(L2) then (10000+hd(L1)-hd(L2))::subtract1(tl(L1),tl(L2),1) else (hd(L1)-hd(L2)) :: subtract1(tl(L1),tl(L2),0);

(*
val compare = int list * int list -> int list

It is called by subtract function.It compare two list then accordingly 
,call subtract1 function with arguments as greater list,smaller list and a borrow (0).  
*)
fun compare([],[])=[]
| compare([h1],[h2])= if h1>h2 then [h1-h2] else [h2-h1]
| compare( L1 as (h1::t1),L2 as (h2::t2) )= if h1=h2 then compare(t1,t2)
                            else if h1>h2 then subtract1(rev(L1),rev(L2),0) else subtract1(rev(L2),rev(L1), 0);

(*
val subtract = int list * int list -> int list

It is called by karatsuba function.It outputs the absolute value of subtraction of two list.
In this, makeequal function is called,followed by compare function and rev function.
*)
fun subtract([],[])=[]
| subtract([h1],[h2])=if h1>h2 then [h1-h2] else [h2-h1]
| subtract(L1,L2)=rev( compare( makeequal(L1,L2) )  );


(*
val add2 = int list *int list * int -> int list

It is called by add2 function.It computes sum of two lists with carry generated at each step during traversal of lists.
*)
fun add2([],[],c)=if c=0 then [] else [c]
| add2(L1,L2,c)= ((hd(L1)+hd(L2)+c) mod 10000) :: add2(tl(L1),tl(L2), ((hd(L1)+hd(L2)+c) div 10000) );


(*
val add1 = int list * int list -> int list

It is called by add function.If any list contain more than one element, 
then it will call add2 function with input as reverse of both lists and carry with value 0.
*)
fun add1([],[])=[]
| add1([h1],[h2]) = if ((h1+h2) div 10000)>0 then ((h1+h2) div 10000)::[((h1+h2) mod 10000)] else [((h1+h2) div 10000)]
| add1(L1,L2)=add2(rev(L1),rev(L2),0);


(*
val add = int list * int list -> int list

It is called by karatsuba function.It output the sum of two given lists.
In this , makeequal function is called, followed by add1 function,then by rev function.
*)
fun add([],[])=[]
| add([h1],[h2])= if ((h1+h2) div 10000)>0 then ((h1+h2) div 10000)::[((h1+h2) mod 10000)] else [((h1+h2) mod 10000)]
| add(L1,L2)= rev(add1(makeequal(L1,L2)));


(*
val mult =int list * int list -> int list

It is called by karatsuba function.It is used to compute the product of two lists having single elements.
*)
fun mult([],[])=[]
| mult([h],[w])=if ((h*w) div 10000)>0 then ((h*w) div 10000)::[(h*w) mod 10000] else [(h*w) mod 10000];


(*
val comparer1 = int list * int list -> bool

It is called by comparer function.It takes two eqaul length lists and compare them.
*)
fun comparer1([h1],[h2])=if h1>=h2 then true else false
|comparer1(L1,L2)= if hd(L1)=hd(L2) then comparer1(tl(L1),tl(L2))
else if hd(L1)>hd(L2) then true
else false;


(*
val comparer = int list * int list -> bool

It is called by compareparts function.In this, makeequal function is called followed by comparer1 function.
*)
fun comparer(L1,L2)= comparer1(makeequal(L1,L2));


(*
val compareparts = int list * int list * int list * int list -> bool

It is called by karatsuba function. It compare the result of comparision of (x1,x0) and comparision of (y1,y0) to give its output.
*)
fun compareparts(L1,L2,L3,L4)=if comparer(L2,L1)=comparer(L3,L4) then true else false;

(*
val makeeqaul1 = int list * int list -> int list * int list

It is called by karatsuba function.It make length of two list equal by using append function.
*)
fun makeequal1 (L1 as (h1::t1),L2 as (h2::t2)) = if length(L1)=length(L2) then (L1,L2)
else if length(L1)>length(L2) then
     let 
       val k=length(L1)-length(L2);
     in
     (L1,append(L2,k))
     end
else 
    let 
      val p=length(L2)-length(L1)
     in
     (append(L1,p),L2)
     end;
     

(*
val karatsuba = int list -> int list -> int list

It is called by factorial2 function (local function of factorial1 function).
It make use of makeequal1, splitting ,padder ,subtract,add and compareparts function to compute the product of two list.
*)     
fun karatsuba [] []=[0]
|karatsuba L []=[0]
|karatsuba [] L=[0]
|karatsuba [x] [y]= mult([x],[y])
|karatsuba L1 L2  = 
   let
        val (l1,l2)=makeequal1(L1,L2)
        val v=length(l1)
        val w= (v+1) div 2
        val (x1,x0)=spliting(l1,w)
        val (y1,y0)=spliting(l2,w)
        val i1=karatsuba x1 y1
        val i0=karatsuba x0 y0
        val z2=padder(i1,2*w)
        val (x2,y2)=makeequal( subtract(x1,x0), subtract(y1,y0) )
        val z11=if compareparts(x1,x0,y1,y0) then add( add(i1,i0) , karatsuba x2 y2 )
                                             else subtract( add(i1,i0) , karatsuba x2 y2 )
        val z1=padder(z11,w)
  
   in 
        add(add(z2,z1),i0)
       
   end;
   
 
 (*
 Invalid_Input_exception is used to handle the strings which contain non-digit characters (Invalid input for factorial function).
 *)
exception Invalid_Input_exception;


(*
val str2 = char list -> char list

It checks each character whether it is a character form of integer digit.It any character is not, then it raises Invalid_Input_exception
*)
fun str2([])=[]
| str2([h])=if Char.compare(h,#"1")=EQUAL orelse Char.compare(h,#"2")=EQUAL orelse 
            Char.compare(h,#"3")=EQUAL orelse Char.compare(h,#"4")=EQUAL orelse
            Char.compare(h,#"5")=EQUAL orelse Char.compare(h,#"6")=EQUAL orelse 
            Char.compare(h,#"7")=EQUAL orelse Char.compare(h,#"8")=EQUAL orelse
            Char.compare(h,#"9")=EQUAL orelse Char.compare(h,#"0")=EQUAL then [] else raise Invalid_Input_exception
            
| str2(h::t)=if Char.compare(h,#"1")=EQUAL orelse Char.compare(h,#"2")=EQUAL orelse  
             Char.compare(h,#"3")=EQUAL orelse Char.compare(h,#"4")=EQUAL orelse
            Char.compare(h,#"5")=EQUAL orelse Char.compare(h,#"6")=EQUAL orelse  
            Char.compare(h,#"7")=EQUAL orelse Char.compare(h,#"8")=EQUAL orelse
            Char.compare(h,#"9")=EQUAL orelse Char.compare(h,#"0")=EQUAL then h::str2(t) else raise Invalid_Input_exception;            


(*
val str1 = string -> string

It is called by factorial function.It calls str2 function to check the validity of string.
*)
fun str1(s)=implode(str2(explode(s)))
  handle Invalid_Input_exception => (print("Invalid Input ,");"Invalid Input");   



(*
val concater = string -> string

It is called by fromString function to make the size of string a multiple of 4.
*)
fun concater(s,0)=s
|   concater(s,k)="0"^(concater(s,k-1));

(*
val makelist  = string -> int list

It is called by fromString function.It takes string 
and divide it into pieces of 4 characters which are then converted
into integer value of base 10000,forming an integer list. 
*)
fun makelist(s) = if size(s)=0 then []
else (valOf(Int.fromString(substring(s,0,4))))::(makelist(substring(s,4,size(s)-4)));


(*
val fromString = string -> int list

It is called by factorial function.It checks whether the size of string is multiple of 4 or not. 
If size is multiple, makelist function is called,otherwise concater function is called,followed by makelist function.
*)
fun fromString s =if (size(s) mod 4)=0 then makelist(s)
else makelist(concater(s,4-(size(s) mod 4)));



(*
val decrement1 = int list -> int list

It is called by decrement function to decrement from head of list.
If head of list is smaller than 1, then decrement1 function will also be applied on tail of list.

*)
fun decrement1(L,0)=L
| decrement1(L,1)=if hd(L)<1 then (hd(L)+9999)::decrement1(tl(L),1) else (hd(L)-1)::decrement1(tl(L),0);


(*
val decrement = int list -> int list

It is called by factorail2 function to decrement the combined value of element in list.If list contain more than one element,it call decrement1 function
with input as reverse of list and integer value(1)
*)
fun decrement([h])=if h=0 then [0] else [h-1]
| decrement(L)=removezero(rev(decrement1(rev(L),1)));

(*
val checklist = int list -> bool

It checks whether all elements in given list are zero or not.If all zeroes then return true,otherwise false 
*)
fun checklist([])=true
| checklist([h])=if h=0 then true else false
| checklist(L as (h::t))=if h=0 then checklist(tl(L)) else false;

(*
val toString1 = int list -> string

As, all elements are in base 10000.Some elements in list can be of 1-digit,
2-digit or 3-digit,they have to be converted into substring of 4 characters
to get desired results.This task is done by toString1 function.
*)

fun toString1([])=""
|   toString1([h])=  if (h div 1000)>0 then Int.toString(h)
                    else if (h div 100)>0 then "0"^Int.toString(h)
                    else if (h div 10)>0 then "00"^Int.toString(h)
                    else  "000"^Int.toString(h)
|   toString1(h::t)= if (h div 1000)>0 then Int.toString(h)^toString1(t)
                    else if (h div 100)>0 then "0"^(Int.toString(h)^toString1(t))
                    else if (h div 10)>0 then "00"^(Int.toString(h)^toString1(t))
                    else  "000"^(Int.toString(h)^toString1(t));
                    
                    
                    
(*
val toString = int list -> string

It takes integer list and return string.For list having more than one element ,it calls toString1 function to process the tail of the list.
*)                    
fun toString([])=""
| toString([h])=Int.toString(h)
| toString(h::t)=Int.toString(h)^toString1(t);

(*
val factorial = int list -> int list

It is called by factorial function.It checks the list whether it contain all zero 
elements or not.It uses factorial2 function to do factorial with the use of karatsuba function.
*)     

fun factorial1(L1) = if checklist(L1) then [1] 
                     else let fun factorial2(L1,L2)=
                       if checklist(L1) then L2
                       else factorial2( decrement(L1) , karatsuba L1 L2 )
                  in 
                  factorial2(L1,[1])
                  end;
                  
(* 
  val factorial = string -> string

   It checks whether the string given is correct or not.If value is not correct then 
   return the string otherwise return the factorial value of given input string 
   
*)     
fun factorial "" = "Invalid Input"
|factorial s=if String.compare(str1(s),"Invalid Input")=EQUAL then s
              else toString(removezero(factorial1 ( fromString s )) ); 
              
              
              
              