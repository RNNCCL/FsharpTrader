module MercadoBitcoin

open RestSharp
open System
open Newtonsoft.Json.Linq
open Utils

let private tapiChave = LoadAppSetting<string> "MB-tapiChave"
let private tapiCodigo = LoadAppSetting<string> "MB-tapiCodigo"
let private tickerUpdateInterval = LoadAppSetting<double> "MB-tickerUpdateInterval"
let private client = new RestClient("https://www.mercadobitcoin.net")
let private tickerRequest = new RestRequest("/api/ticker/", Method.GET)

type private msg = 
    | Fetch of AsyncReplyChannel<JObject>

let private getTickerInternal = 
    MailboxProcessor.Start(fun inbox -> 
        let rec loop (lastRequestTime: DateTime) lastResult = 
            async { 
                let! (Fetch replyChannel) = inbox.Receive()
                match DateTime.Now.Subtract(lastRequestTime).TotalSeconds < tickerUpdateInterval with
                | true -> 
                    replyChannel.Reply lastResult
                    return! loop lastRequestTime lastResult
                | false -> 
                    printfn "\n- Updating Mercado Bitcoin prices"
                    let response = client.Execute(tickerRequest)
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

type ticker = 
    { buy: double
      sell: double }

let GetTicker() = 
    let json = getTickerInternal.PostAndReply(fun replyChannel -> Fetch replyChannel)
    { buy = json.["ticker"].["buy"].ToObject<double>()
      sell = json.["ticker"].["sell"].ToObject<double>() }
