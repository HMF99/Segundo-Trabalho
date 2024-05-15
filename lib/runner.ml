type command = Push of int | Pop | Dup | Swp | Over | Add | Sub | Mul | Div | Cmp | Jmp of string | Jz of string | Jnz of string | Return


module LeitorComandos = struct

  let push x stack =  x::stack

  let pop stack = 
    match stack with
    | [] -> failwith "Unexpected empty stack"
    | _h::t -> t

  let dup stack = 
    match stack with
    | [] -> failwith "Unexpected empty stack"
    | h::_t -> h::stack
    
  let swp stack = 
    match stack with
    | [] -> failwith "Unexpected empty stack"
    | [_] -> failwith "Unexpected empty stack"
    | h::h2::t -> h2::h::t

  let over stack = 
    match stack with
    | [] -> failwith "Unexpected empty stack"
    | _h::h2::_t -> h2::stack
    | _ -> failwith "Unexpected empty stack"

  let add stack = 
    match stack with
    | [] -> failwith "Unexpected empty stack"
    | h::h2::t -> (h+h2)::t
    | [_]-> failwith "Unexpected empty stack"  
    
  let sub stack = 
    match stack with
    | [] -> failwith "Unexpected empty stack"
    | h::h2::t -> (h2-h)::t
    | [_] -> failwith "Unexpected empty stack"
    
  let mul stack = 
    match stack with
    | [] -> failwith "Unexpected empty stack"
    | h::h2::t -> (h*h2)::t
    | [_] -> failwith "Unexpected empty stack"
    
  let div stack = 
    match stack with
    | [] -> failwith "Unexpected empty stack"
    | h::h2::t -> if h2 = 0 then failwith "Division by zero" else (h/h2)::t
    | [_] -> failwith "Unexpected empty stack"

  let cmp stack = 
    match stack with
    | [] -> failwith "Unexpected empty stack"
    | h::h2::t -> if h = h2 then 1::t else 0::t
    | [_] -> failwith "Unexpected empty stack"

end


module StringMap = Map.Make(String) 


module Interpretor = struct

  let match_command x temp = 
    match String.trim x with
    | "POP" -> Pop :: temp
    | "DUP" -> Dup :: temp
    | "SWP" -> Swp :: temp
    | "OVER" -> Over :: temp
    | "ADD" -> Add :: temp
    | "SUB" -> Sub :: temp
    | "MUL" -> Mul :: temp
    | "DIV" -> Div :: temp
    | "CMP" -> Cmp :: temp
    | _ -> match String.split_on_char ' ' (String.trim x) with
        | "PUSH"::int::_tail -> Push (int_of_string int) :: temp
        | "JMP"::label::_tail -> if List.length temp > 1 
                                   then failwith "Expecting label" 
                                 else 
                                   Jmp label :: temp
        | "JZ"::label::_tail -> Jz label :: temp
        | "JNZ"::label::_tail -> Jnz label :: temp
        | "RETURN"::_tail -> Return :: temp
        | _ -> failwith "Invalid instruction"



  let rec read_commands lines command_list = 
      let rec read l temp =
        match l with 
        | [] -> if List.length temp > 1 then failwith "Expecting label" else  command_list
        | h::t -> match String.split_on_char ':' h with 
                   | _::_::_::_tail-> failwith "Invalid line"
                   | [] -> read_commands t command_list
                   | label::command::_tail -> read_commands (("JMP "^label)::t) (StringMap.add label (match_command command temp) command_list)
                   | command::_tail -> read t (match_command command temp)
      in read lines []
      
              



  let rec start_run_commands current_label command_list i_stack =
    try (let block = StringMap.find current_label command_list  in
     let rec run_commands b stack = 
      
        match b with 
          | [] -> failwith "Label not found"
          | h::t ->  match h with
                  | Return -> if List.length stack = 1 then List.hd stack else failwith "Stack not empty"
                  | Push (i) ->  run_commands t (LeitorComandos.push i stack)
                  | Pop -> run_commands t (LeitorComandos.pop stack)
                  | Dup -> run_commands t  (LeitorComandos.dup stack)
                  | Swp -> run_commands t (LeitorComandos.swp stack)
                  | Over -> run_commands t (LeitorComandos.over stack)
                  | Add -> run_commands t (LeitorComandos.add stack)
                  | Sub -> run_commands t (LeitorComandos.sub stack)
                  | Mul -> run_commands t (LeitorComandos.mul stack)
                  | Div -> run_commands t (LeitorComandos.div stack)
                  | Cmp -> run_commands t (LeitorComandos.cmp stack)
                  | Jz (s) -> if (List.hd stack) = 0 then start_run_commands s command_list (LeitorComandos.pop stack)  else run_commands t (LeitorComandos.pop stack)
                  | Jnz (s) -> if (List.hd stack) != 0 then start_run_commands s command_list (LeitorComandos.pop stack)  else run_commands t (LeitorComandos.pop stack)
                  | Jmp (s) -> start_run_commands s command_list stack
     in run_commands block i_stack
    ) with Not_found -> failwith ("Label not found:" ^ current_label)
end


let run lines label =
    let stack = [] in
      if List.length lines = 0 
        then failwith ("Label not found:" ^ label) 
      else
        let l = List.rev lines in
          if compare (List.hd l) "RETURN" != 0 
            then failwith "No return instruction" 
          else
            let command_list = Interpretor.read_commands l StringMap.empty in
              Interpretor.start_run_commands label command_list stack
