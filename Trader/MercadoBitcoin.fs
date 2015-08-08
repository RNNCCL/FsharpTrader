module MercadoBitcoin

open RestSharp
open Utils
open System.Threading
open Newtonsoft.Json

let private tapiChave = LoadAppSetting<string> "MB-tapiChave"
let private tapiCodigo = LoadAppSetting<string> "MB-tapiCodigo"
let private tickerUpdateInterval = LoadAppSetting<int> "MB-tickerUpdateInterval"
let private client = new RestClient("https://www.mercadobitcoin.net")
let private tickerRequest = new RestRequest("/api/ticker/", Method.GET)

type tickerResponse = 
    { ticker : ticker }

and ticker = 
    { buy : decimal
      sell : decimal }

type private msg = 
    | Store of ticker
    | Fetch of AsyncReplyChannel<ticker>

let private tickerManager = 
    MailboxProcessor.Start(fun inbox -> 
        let rec loop ticker = 
            async { 
                let! msg = inbox.Receive()
                match msg with
                | Store newTicker -> 
                    return! loop newTicker
                | Fetch replyChannel -> 
                    replyChannel.Reply ticker
                    return! loop ticker
            }
        loop { buy = 0m
               sell = 0m })

let rec private DownloadAndStoreTicker() = 
    let response = client.Execute(tickerRequest)
    match response.ErrorException with
    | :? System.Net.WebException -> 
        Thread.Sleep(30 * 1000)
        DownloadAndStoreTicker()
    | null -> 
        let parse = JsonConvert.DeserializeObject<tickerResponse>(response.Content)
        TerminalDispatcher.PrintInfo "Updating data from MercadoBitcoin"
        tickerManager.Post(Store parse.ticker)
    | _ -> raise response.ErrorException

let rec private DownloadService() = 
    async { 
        Thread.Sleep(tickerUpdateInterval * 1000)
        DownloadAndStoreTicker()
        return! DownloadService()
    }

let Initialize() =
    DownloadAndStoreTicker()
    Async.Start (DownloadService())
    
let GetTicker() = 
    tickerManager.PostAndReply(fun replyChannel -> Fetch replyChannel)
