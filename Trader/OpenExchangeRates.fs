module OpenExchangeRates

open RestSharp
open System
open Newtonsoft.Json.Linq
open Utils
open System.Threading
open Newtonsoft.Json

let private appId = LoadAppSetting<string> "OXR-appId"
let private updateInterval = LoadAppSetting<int> "OXR-updateInterval"
let private client = new RestClient("https://openexchangerates.org")
let private request = new RestRequest("/api/latest.json", Method.GET)

request.AddParameter("app_id", appId) |> ignore

type ratesResponse = 
    { rates : rates }

and rates = 
    { BRL : decimal
      NZD : decimal }

type private msg = 
    | Store of rates
    | Fetch of AsyncReplyChannel<rates> 

let private ratesManager = 
    MailboxProcessor.Start(fun inbox -> 
        let rec loop rates = 
            async { 
                let! msg = inbox.Receive()
                match msg with
                | Store newRates -> 
                    return! loop newRates
                | Fetch replyChannel -> 
                    replyChannel.Reply rates
                    return! loop rates
            }
        loop { BRL = 0m
               NZD = 0m })

let rec private DownloadAndStoreTicker() = 
    let response = client.Execute(request)
    match response.ErrorException with
    | :? System.Net.WebException -> 
        Thread.Sleep(30 * 1000)
        DownloadAndStoreTicker()
    | null -> 
        let parse = JsonConvert.DeserializeObject<ratesResponse>(response.Content)
        TerminalDispatcher.PrintInfo "Updating data from OpenExchangeRates"
        ratesManager.Post(Store parse.rates)
    | _ -> raise response.ErrorException

let rec private DownloadService() = 
    async { 
        Thread.Sleep(updateInterval * 1000)
        DownloadAndStoreTicker()
        return! DownloadService()
    }

let Initialize() =
    DownloadAndStoreTicker()
    Async.Start (DownloadService())

let GetNzdBrlExchange() = 
    let rates = ratesManager.PostAndReply(fun replyChannel -> Fetch replyChannel)
    rates.BRL / rates.NZD
