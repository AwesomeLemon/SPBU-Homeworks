﻿module Interpreter
open Parser
open Expression
open System

type IInputProvider =
  interface
    abstract Read : unit -> int
  end

type ListInput (list) = 
  let mutable list = list
  interface IInputProvider with
    override self.Read () =
      let mutable res = 0
      match list with
      | [] -> raise(Error("Not enough variables values"))
      | x :: l -> 
        res <- x
        list <- l
      res

type ConsoleInput () =
  interface IInputProvider with
    override self.Read () = 
      int (Console.ReadLine ())

type IOutputProvider =
  interface
    abstract Write : string -> unit
  end

type ListOutput () = 
  let mutable list = []
  member self.Out () =
      List.rev list
  interface IOutputProvider with
    override self.Write (num) =
      list <- num :: list
    
      
type ConsoleOutput() = 
  interface IOutputProvider with
    override self.Write (s) =
      printf "%s" s

let interprete (tree : tree) (input : IInputProvider) (output : IOutputProvider) = 
  let (vars: (string * int)[]) = [||]

  let rec calcStep (tree : tree) (vars: (string * int)[]) =
    let mutable vars = vars

    match tree with
    | Read (s) ->
      let varExists elem =  
        s = fst elem
      if (Array.exists varExists vars) then
        let a = Array.findIndex varExists vars
        vars.[a] <- match vars.[a] with
                          | (s, _) -> (s, input.Read())
      else
        vars <- Array.append vars [|(s, input.Read())|]
    | Write (t) ->
      output.Write ((calculateTree t vars).ToString())
    | Seq(l, r) ->
      vars <- calcStep l vars 
      vars <- calcStep r vars 
    | Assign (s, t) ->
      let varExists elem =  
        s = fst elem
      if (Array.exists varExists vars) then
        let a = Array.findIndex varExists vars
        vars.[a] <- match vars.[a] with
                          | (s, _) -> (s, (calculateTree t vars))
      else
        vars <- Array.append vars [|(s, (calculateTree t vars))|]
    | If (t, ifThen, ifElse) ->
      if (calculateTree t vars) <> 0 then
        vars <- calcStep ifThen vars 
      else
        vars <- calcStep ifElse vars 
    | While (t, exp) ->
      while (calculateTree t vars) <> 0 do
        vars <- calcStep exp vars 
    | Nil -> raise(Error("Some command in the input program is missing"))
    vars

  calcStep tree vars |> ignore

[<EntryPoint>]
let main argv = 
  0