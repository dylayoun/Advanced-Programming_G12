// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

type terminal = 
    Add | Sub | UnarySub | Mul | Div | Mod | Exp | Dot | Lpar | Rpar | Num of int

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let parseError = System.Exception("Parser error")
let divideByZeroError = System.Exception("Division by zero")


// get how many of the first characters in a string are 0s
// used for reading decimals
// count should have an initial value of 0 passed in
let rec getLeadingZeroes(iStr, count) =
    match iStr with
    | '0' :: tail -> getLeadingZeroes(tail, count+1)
    | _ -> (iStr, count)

let rec removeTrailingZeroes(num) = 
    if (num % 10 = 0) then
        removeTrailingZeroes(num / 10)
    else
        num

let rec scInt(iStr, iVal) = 
    match iStr with
    | c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)

// convert two integers a, b into double a.b
let intsToDouble(a:int, b:int) =
    match b with
    | 0 -> (double) a
    | _ -> let digitsInB = floor(log10((double)b))+1.0
           let truncB = removeTrailingZeroes(b)
           (double)a + (double)truncB*(10.0 ** -(digitsInB))



let lexer input = 
    let rec scan input last_was_digit =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail false
        | '-'::tail -> match last_was_digit with
                        | true -> Sub :: scan tail false
                        | false -> UnarySub :: scan tail false
        | '*'::tail -> Mul :: scan tail false
        | '/'::tail -> Div :: scan tail false
        | '%'::tail -> Mod :: scan tail false
        | '^'::tail -> Exp :: scan tail false
        | '('::tail -> Lpar:: scan tail false
        | ')'::tail -> Rpar:: scan tail false
        | '.'::tail -> Dot :: scan tail true
        | c :: tail when isblank c -> scan tail last_was_digit

        // it seems a bit odd but in order to store leading 0s in decimal numbers they are moved to the end of the number
        // only if the last symbol was a number (i.e. this is a decimal)
        | c :: tail when isdigit c -> match last_was_digit with
                                      | true -> let (iStr, count) = getLeadingZeroes(tail, 0)
                                                let (iStr, iVal) = scInt(tail, intVal c)
                                                let modified_iVal = iVal * (int)(10.0 ** ((double)count+1.0))
                                                Num modified_iVal :: scan iStr true
                                      | false -> let (iStr, iVal) = scInt(tail, intVal c)
                                                 Num iVal :: scan iStr true
        | _ -> raise lexError
    scan (str2lst input) false

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Grammar in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>        ::= <NR> <Popt>
// <Popt>     ::= "^" <NR> <Popt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")" | "- (unary)" <NR> | <value> '.' <value>

let parser tList = 
    let rec E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
    and Eopt tList = 
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList

    and T tList = (P >> Topt) tList
    and Topt tList =
        match tList with
        | Mul :: tail -> (P >> Topt) tail
        | Div :: tail -> (P >> Topt) tail
        | Mod :: tail -> (P >> Topt) tail
        | _ -> tList

    and P tList = (NR >> Popt) tList
    and Popt tList = 
        match tList with
        | Exp :: tail -> (NR >> Popt) tail
        | _ -> tList

    and NR tList =
        match tList with 
        | Num value :: tail -> match tail with
                               | Dot :: Num subvalue :: subtail -> subtail  // float
                               | Dot :: subtail -> raise parseError         // incomplete float
                               | _ -> tail                                  // int
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | UnarySub :: tail -> (NR) tail
        | _ -> raise parseError
    E tList

// every time values are passed around they are followed by a vIsInt argument
// this is a boolean. True = the value is an int, False = the value is a double
// this is used for integer arithmetic as all values are internally doubles

// for integer arithmetic mode to be active both must have it set to True
// though most operations don't actually change except division

let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt(tList, value: double, vIsInt) = 
        match tList with
        | Add :: tail -> let (tLst, tval, tvIsInt) = T tail
                         Eopt (tLst, value + tval, tvIsInt && vIsInt)
        | Sub :: tail -> let (tLst, tval, tvIsInt) = T tail
                         Eopt (tLst, value - tval, tvIsInt && vIsInt)
        | _ -> (tList, value, vIsInt)

    and T tList = (P >> Topt) tList
    and Topt (tList, value: double, vIsInt) =
        match tList with
        | Mul :: tail -> let (tLst, tval, tvIsInt) = P tail
                         Topt (tLst, value * tval, tvIsInt && vIsInt)
        | Div :: tail -> let (tLst, tval, tvIsInt) = P tail
                         match tval with
                         | 0.0 -> raise divideByZeroError // check for division by 0
                         | _ -> if (tvIsInt && vIsInt) then
                                    Topt (tLst, floor(value / tval), tvIsInt && vIsInt) // integer division
                                else
                                    Topt (tLst, value / tval, tvIsInt && vIsInt)        // standard division
        | Mod :: tail -> let (tLst, tval, tvIsInt) = P tail
                         Topt (tLst, value % tval, tvIsInt && vIsInt)
        | _ -> (tList, value, vIsInt)

    and P tList = (NR >> Popt) tList
    and Popt (tList, value: double, vIsInt) =
        match tList with
        | Exp :: tail -> let (tLst, tval, tvIsInt) = NR tail
                         let result = (value ** tval)
                         Popt (tLst, result, vIsInt && tvIsInt)
        | _ -> (tList, value, vIsInt)

    and NR tList =
        match tList with 
        | Num value :: tail -> match tail with
                               | Dot :: Num subvalue :: subtail -> (subtail, intsToDouble(value,subvalue), false) // float
                               | Dot :: subtail -> raise parseError                                                             // incomplete float
                               | _ -> (tail, value, true)                                                                       // int
        | Lpar :: tail -> let (tLst, tval, tvIsInt) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval, tvIsInt)
                          | _ -> raise parseError
        | UnarySub :: tail -> let (tLst, tval, tvIsInt) = T tail
                              Eopt (tLst, 0.0 - tval, tvIsInt)
        | _ -> raise parseError
    E tList

let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []


[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter")
    let input:string = getInputString()
    let oList = lexer input
    let sList = printTList oList;
    let pList = printTList (parser oList)
    let tList, Out, isInt = parseNeval oList
    Console.WriteLine("Result = {0}", Out)
    0
