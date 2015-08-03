module BitNZ

open RestSharp
open System
open System.Security.Cryptography
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Utils

let private username = LoadAppSetting<string> "BNZ-username"
let private apiKey = LoadAppSetting<string> "BNZ-apiKey"
let private apiSecret = LoadAppSetting<string> "BNZ-apiSecret"
let private mbWallet = LoadAppSetting<string> "MB-wallet"
let private client = new RestClient("https://bitnz.com")
let private requestOrderbook = new RestRequest("/api/0/orderbook")
let private urlBalance = "/api/0/private/balance"
let private urlBtcWithdraw = "/api/0/private/btc/withdraw"
let private urlOpenBuyOrders = "/api/0/private/orders/buy/open"
let private urlCancelBuyOrder = "/api/0/private/orders/buy/cancel"
let private urlCreateBuyOrder = "/api/0/private/orders/buy/create"

type balance = 
    { nzd_balance: decimal
      btc_balance: decimal
      nzd_reserved: decimal
      btc_reserved: decimal
      nzd_available: decimal
      btc_available: decimal }

type order = 
    { id: int
      price: decimal
      amount: decimal }

let private HashSHA256(message: string) = 
    let encoding = new ASCIIEncoding()
    let sha256 = new HMACSHA256(encoding.GetBytes(apiSecret))
    let hash = sha256.ComputeHash(encoding.GetBytes(message))
    BitConverter.ToString(hash).Replace("-", "")

let private GetBaseRequest(url: string) = 
    let request = new RestRequest(url, Method.POST)
    let span = DateTime.Now.Subtract(new DateTime(2015, 07, 31))
    let nonce = Convert.ToInt32(span.TotalSeconds).ToString()
    let message = nonce + username + apiKey
    let signature = HashSHA256 message
    request.AddParameter("key", apiKey).AddParameter("nonce", nonce).AddParameter("signature", signature)

let private ExecuteRequest request = 
    let response = client.Execute(request)
    match response.StatusCode with
    | Net.HttpStatusCode.OK -> response.Content
    | _ -> raise response.ErrorException

let GetOrderbook() = 
    let response = ExecuteRequest requestOrderbook
    let json = JObject.Parse(response)
    
    let mapOrders t = 
        json.[t].Children() |> Seq.map (fun x -> 
                                   { id = 0
                                     price = Convert.ToDecimal(x.[0])
                                     amount = Convert.ToDecimal(x.[1]) })
    (mapOrders "bids", mapOrders "asks")

let GetBalance() = 
    let request = GetBaseRequest(urlBalance)
    let response = ExecuteRequest request
    JsonConvert.DeserializeObject<balance>(response)

let GetBuyOrders() = 
    let request = GetBaseRequest(urlOpenBuyOrders)
    let response = ExecuteRequest request
    JsonConvert.DeserializeObject<order []>(response) 

exception TransactionException of string

let private ExecuteTransaction' request tx error = 
    let response = ExecuteRequest request
    let json = JObject.Parse(response)
    match Convert.ToBoolean(json.["result"]) with
    | false -> raise (TransactionException error)
    | true -> printfn "%s" tx

let private ExecuteTransaction request tx error = 
    match true with
    | false -> raise (TransactionException error)
    | true -> printfn "%s" tx

let CreateBuyOrder (price : decimal) (amount : decimal) msg = 
//    let request = GetBaseRequest(urlCreateBuyOrder)
//    request.AddParameter("price", Math.Round(price, 8)) |> ignore
//    request.AddParameter("amount", Math.Round(amount, 8)) |> ignore
//    let response = ExecuteRequest request
//    let json = JObject.Parse(response)
//    match Convert.ToBoolean(json.["result"]) with
//    | false -> 
//        let error = "Error creating order: " //+ json.["result"].["message"].ToString()
//        raise (TransactionException error)
//    | true -> 
        TerminalDispatcher.PrintCreateOrder price amount msg
//         TODO: Parse from json
        { id = 99; price = price; amount = amount }




    

let CancelOrder order msg = 
    let request = GetBaseRequest(urlCancelBuyOrder)
    request.AddParameter("id", order.id) |> ignore
    TerminalDispatcher.PrintCancelOrder order.price order.amount msg
