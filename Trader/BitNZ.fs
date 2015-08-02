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
let private requestOrderbook = new RestRequest("/api/0/orderbook?group=0")
let private urlBalance = "/api/0/private/balance"
let private urlBtcWithdraw = "/api/0/private/btc/withdraw"
let private urlOpenBuyOrders = "/api/0/private/orders/buy/open"
let private urlCancelBuyOrder = "/api/0/private/orders/buy/cancel"
let private urlCreateBuyOrder = "/api/0/private/orders/buy/create"

type balance = 
    { nzd_balance: float
      btc_balance: float
      nzd_reserved: float
      btc_reserved: float
      nzd_available: float
      btc_available: float
      fee: string }

type order = 
    { id: int
      price: float
      amount: float }

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
                                     price = Convert.ToDouble(x.[0])
                                     amount = Convert.ToDouble(x.[1]) })
    (mapOrders "bids", mapOrders "asks")

let GetBalance() = 
    let request = GetBaseRequest(urlBalance)
    let response = ExecuteRequest request
    JsonConvert.DeserializeObject<balance>(response)

let GetBuyOrders() = 
    let request = GetBaseRequest(urlOpenBuyOrders)
    let response = ExecuteRequest request
    JsonConvert.DeserializeObject<order []>(response) |> Seq.sortByDescending (fun x -> x.price)

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

let CreateBuyOrder price amount = 
    let request = GetBaseRequest(urlCreateBuyOrder)
    request.AddParameter("price", price) |> ignore
    request.AddParameter("amount", amount) |> ignore
    let tx = sprintf "+ %.8f  %.8f" price amount
    ExecuteTransaction request tx "Error creating order"

let CancelOrder order = 
    let request = GetBaseRequest(urlCancelBuyOrder)
    request.AddParameter("id", order.id) |> ignore
    let tx = sprintf "- %.8f  %.8f" order.price order.amount
    ExecuteTransaction request tx "Error canceling order"
