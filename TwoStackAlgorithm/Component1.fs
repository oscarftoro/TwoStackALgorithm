namespace TwoStackAlgorithm

open System
open System.Collections


//cute Djikstra Two Stack's Algorithm
module TwoStacksAlg =

    let expr1 = "(1+((2+3)*(4*5)))"
    let array = expr1.ToCharArray() |> Array.toList

    let c= 'c'
    let isNum ch = Char.IsNumber(ch)

     // six pomodoros and list of operators but not a list of operands
    // take a list of expressions and create a list of operands and a list of operators
    let rec lister list ops vals = 
            match list with
              | hd::hd1::tl when isNum hd && not (isNum hd1)               -> lister tl (hd1::ops) (hd::vals)
              | hd::hd1::tl when (isNum hd || hd.Equals('.')) && isNum hd1 -> lister tl  ops (hd1::hd::vals)   
              | hd::hd1::tl when not (isNum hd) && isNum hd1               -> lister tl (hd::ops) (hd1::vals)
              | hd::tl      when not(isNum hd)                             -> lister tl (hd::ops) vals
              | hd::tl      when isNum hd  || hd.Equals('.')               -> lister tl  ops (hd::vals)        
              | ([] : char list)                                           -> ((ops: char list),(vals: char list))
              | _                                                          -> raise (System.ArgumentException("Error on list argument"))

    let list = expr1.ToCharArray() |> Array.toList
    Console.Out.WriteLine("hola");
    //push operands onto the operand stack
    //push operators onto the operator stack
    //ignore left parentheses
    //on encounterng right parenthesis
    //  - pop operator
    //  - pop requisite number of operands
    //  - push onto the operand stack the result of applying the operatos to those operands
   

    // i need to parse the string in order to evaluate the numbers
    //otherwise is not going to work

    let rec eval expr ops vals = 
        match expr with
         | " ":: t -> eval t ops vals
         | "(":: t -> eval t ops vals
         | "+":: t -> eval t ('+' :: ops) vals
         | "*":: t -> eval t ('*' :: ops) vals
         | x  :: t -> eval t ops (Double.Parse(x):: vals)
         | []      -> (ops,vals)

        
 
  
    

