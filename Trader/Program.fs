// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Configuration
open System.Threading

[<EntryPoint>]
let main argv = 
    printfn "Warming up..."
    MercadoBitcoin.Initialize()
    OpenExchangeRates.Initialize()
    
    TraderBot.Loop()


    printfn "acabou"
    Console.Read()
    0 // return an integer exit code
