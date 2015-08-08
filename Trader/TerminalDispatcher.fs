module TerminalDispatcher

open System

type Color = 
    | Black = 30
    | Red = 31
    | Green = 32
    | Yellow = 33
    | Blue = 34
    | Magenta = 35
    | Cyan = 36
    | White = 37

type Intensity = 
    | Normal = 0
    | Bright = 2


type OrderOwner =
    | Me
    | ThirdPart
    | Group

let IsLinux = System.Type.GetType("Mono.Runtime") <> null

let ResetCode = 
    if IsLinux then "\x1B[m"
    else String.Empty

let GetColorSequence color level =
    if IsLinux then sprintf "\x1B[%d;%dm" level color
    else String.Empty


let GetColoredValue (value : decimal) color =
    let c = GetColorSequence color
    let arr =
        (sprintf "%14.8f" value).ToCharArray()
        |> Array.rev
        |> Array.fold (fun (tail, valueStarted) x -> 
           let s, v = 
               if (x >= '1' && x <= '9') || valueStarted then 
                    sprintf "%s%c" (c 0) x, true
               elif x = '.' then
                    sprintf "%s%c" (c 2) x, true
               else 
                    sprintf "%s%c" (c 2) x, valueStarted
           s :: tail, v) ([], false)

    String.Concat(Array.ofList (fst arr))

let PrintBalanceData item value =
    printfn " %-13s:%s%s" item (GetColoredValue value (int Color.White)) ResetCode

let PrintInfo info =
    let timestamp = DateTime.Now.ToString("HH:mm:ss")
    printfn "%s[%s] %s%s"(GetColorSequence (int Color.Cyan) 0) timestamp info ResetCode
    
let PrintOrderList (price: decimal)  (amount: decimal)  owner =
    let color = 
        match owner with
        | Me -> int Color.Green
        | Group -> int Color.Yellow
        | _ -> int Color.White

    let p1 = (GetColoredValue price color)
    let p2 = (GetColoredValue amount color)
    printfn " %s%s%s"  p1 p2 ResetCode
    

let PrintTransaction color sign price amount tag =
    let p1 = (GetColoredValue price color)
    let p2 = (GetColoredValue amount color)
    let prefix = (GetColorSequence color 0) + sign
    let suffix = (GetColorSequence color 0) + tag
    printfn "%s%s%s [%s]%s" prefix p1 p2 suffix ResetCode

   
let PrintCreateOrder (price: decimal)  (amount: decimal) tag =
    let color = int Color.Blue
    PrintTransaction color "+" price amount tag

let PrintCancelOrder (price: decimal)  (amount: decimal) tag =
    let color = int Color.Red
    PrintTransaction color "-" price amount tag

