module OpenExchangeRates

open RestSharp
open System
open Newtonsoft.Json.Linq
open Utils

let private appId = LoadAppSetting<string> "OXR-appId"
let private updateInterval = LoadAppSetting<double> "OXR-updateInterval"
let private client = new RestClient("https://openexchangerates.org")
let private request = new RestRequest("/api/latest.json", Method.GET)

request.AddParameter("app_id", appId) |> ignore

type private msg = 
    | Fetch of AsyncReplyChannel<JObject>

let private getRates = 
    MailboxProcessor.Start(fun inbox -> 
        let rec loop (lastRequestTime: DateTime) lastResult = 
            async { 
                let! (Fetch replyChannel) = inbox.Receive()
                match DateTime.Now.Subtract(lastRequestTime).TotalSeconds < updateInterval with
                | true -> 
                    replyChannel.Reply lastResult
                    return! loop lastRequestTime lastResult
                | false -> 
                    TerminalDispatcher.PrintInfo "!Updating exchange rates"
                    let response = client.Execute(request)
                    match response.StatusCode with
                    | Net.HttpStatusCode.OK -> 
                        let newResult = JObject.Parse(response.Content)
                        replyChannel.Reply newResult
                        return! loop DateTime.Now newResult
                    | _ when lastResult = null -> raise response.ErrorException
                    | _ -> 
                        printfn "  Error!"
                        replyChannel.Reply lastResult
                        return! loop lastRequestTime lastResult
            }
        loop (new DateTime(2015, 01, 01)) null)

let GetNzdBrlExchange() = 
    let json = getRates.PostAndReply(fun replyChannel -> Fetch replyChannel)
    json.["rates"].["BRL"].ToObject<decimal>() / json.["rates"].["NZD"].ToObject<decimal>()
