namespace TwoStackAlgorithm

open System
open System.Collections


//cute Djikstra Two Stack's Algorithm
module TwoStacksAlg =

    let expr1 = "(1+((2+3)*(4*5)))"

    let list1 = expr1.ToCharArray() |> Array.toList
  

    let isNum ch = Char.IsNumber(ch)

    let drop i ls = List.rev ls |> List.take (ls.Length - i ) |> List.rev

    //on encounterng right parenthesis...
    //   - pop operator
    //   - pop requisite number of operands
    //   - push onto the operand stack the result of applying the operatos to those operands
    let evalOp ops vals =
        match ops with
            | '+':: tl -> (List.tail(ops), List.head(vals) + (List.tail(vals) |> List.head |> float) :: drop 2 (vals))
            | '*':: tl -> (List.tail(ops), List.head(vals) * (List.tail(vals) |> List.head |> float) :: drop 2 (vals))
            | _ -> raise (System.ArgumentException("Not supported evalVal Operator")) 

    //push operands onto the operand stack
    let rec evalVal hd ops vals =
        match hd with
            | ' ' -> (ops,vals)
            | '(' -> (ops,vals)
            | '+' -> ('+'::ops,vals)
            | '*' -> ('*'::ops,vals)
            | ')' -> evalOp ops vals    //on encounterng right parenthesis...
            | _  -> raise (System.ArgumentException("Not supported evalVal Operator")) 

    
    let rec twoStack list ops (vals : float list) =
        match list with
            | hd::tl when isNum(hd)       -> twoStack tl ops (Double.Parse((hd.ToString()))::vals) //push operators onto the operator stack
            | hd::tl when not (isNum(hd)) -> let (ops1,vals1) = (evalVal hd ops vals)
                                             in twoStack tl ops1 vals1
            | ([]: char list)             -> vals
            | _                           -> raise (System.ArgumentException("fail"))

  
 
   

  
    

