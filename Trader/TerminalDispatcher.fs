module TerminalDispatcher

open System

type private Color = 
    | Black = 30
    | Red = 31
    | Green = 32
    | Yellow = 33
    | Blue = 34
    | Magenta = 35
    | Cyan = 36
    | White = 37

type private Intensity = 
    | Normal = 0
    | Bright = 2


let private IsLinux = System.Type.GetType("Mono.Runtime") <> null

let private ResetCode = 
    if IsLinux then "\x1B[m"
    else String.Empty

let private GetColorSequence color level =
    if IsLinux then sprintf "\x1B[%d;%dm" (int level) (int color)
    else String.Empty

let private GetColoredValue (value : decimal) color =
    let c = GetColorSequence color

    if value > 0m then
        let arr =
            (sprintf "%14.8f" value).ToCharArray()
            |> Array.rev
            |> Array.fold (fun (tail, valueStarted) x -> 
               let s, v = 
                   if (x >= '1' && x <= '9') || valueStarted then 
                        sprintf "%s%c" (c Intensity.Normal) x, true
                   elif x = '.' then
                        sprintf "%s%c" (c Intensity.Bright) x, true
                   else 
                        sprintf "%s%c" (c Intensity.Bright) x, valueStarted
               s :: tail, v) ([], false)

        String.Concat(Array.ofList (fst arr))
    else
        sprintf "%s%s" (c Intensity.Bright) (sprintf "%14.8f" value)

let private PrintTransaction color sign price amount tag =
    let p1 = (GetColoredValue price color)
    let p2 = (GetColoredValue amount color)
    let prefix = (GetColorSequence color Intensity.Normal) + sign
    let suffix = (GetColorSequence color Intensity.Normal) + tag
    printfn "%s%s%s [%s]%s" prefix p1 p2 suffix ResetCode

let PrintBalanceData item value =
    printfn " %-13s:%s%s" item (GetColoredValue value Color.White) ResetCode

let PrintInfo info =
    let timestamp = DateTime.Now.ToString("HH:mm:ss")
    printfn "%s[%s] %s%s"(GetColorSequence Color.Cyan Intensity.Normal) timestamp info ResetCode
    
type OrderOwner =
    | Me
    | ThirdPart
    | Group    
    
let PrintOrderList (price: decimal)  (amount: decimal) owner =
    let color = 
        match owner with
        | Me -> Color.Green
        | Group -> Color.Yellow
        | _ -> Color.White

    let p1 = (GetColoredValue price color)
    let p2 = (GetColoredValue amount color)
    printfn " %s%s%s"  p1 p2 ResetCode
   
let PrintCreateOrder (price: decimal)  (amount: decimal) tag =
    PrintTransaction Color.Blue "+" price amount tag

let PrintCancelOrder (price: decimal)  (amount: decimal) tag =
    PrintTransaction Color.Red "-" price amount tag

let PrintWithdraw (amount: decimal) = 
    let timestamp = DateTime.Now.ToString("HH:mm:ss")
    printfn "%s[%s] WITHDRAW: %f%s"(GetColorSequence Color.Red Intensity.Normal) timestamp amount ResetCode