﻿module TraderBot

open Utils
open System
open System.Threading

let private totalFees = LoadAppSetting<double> "totalFees"
let private maxAmount = LoadAppSetting<double> "maxAmount"
let private frenzyTime = LoadAppSetting<double> "frenzyTime"
let private sleepTime = LoadAppSetting<int> "sleepTime"

type private msg1 = 
    | SetFrenzyMode
    | IsFrenzyMode of AsyncReplyChannel<bool>

type private msg2 = 
    | StoreBalance of BitNZ.balance
    | StoreWithdraw of double
    | FetchBalance of AsyncReplyChannel<double * double>
    | FetchWithdraw of AsyncReplyChannel<double>

type private msg3 = 
    | FetchPrice of AsyncReplyChannel<double>

let private frenzyManager = 
    MailboxProcessor.Start(fun inbox -> 
        let rec loop lastTimeSet = 
            async { 
                let! msg = inbox.Receive()
                match msg with
                | SetFrenzyMode -> return! loop DateTime.Now
                | IsFrenzyMode replyChannel -> 
                    replyChannel.Reply(DateTime.Now.Subtract(lastTimeSet).TotalSeconds < frenzyTime)
                    return! loop lastTimeSet
            }
        loop (new DateTime(2015, 01, 01)))

let private balanceManager = 
    MailboxProcessor.Start(fun inbox -> 
        let rec loop initialNzdBalance initialBtcBalance btcWithdraw = 
            async { 
                let! msg = inbox.Receive()
                match msg with
                | StoreBalance balance -> 
                    match initialNzdBalance = 0.0 && initialBtcBalance = 0.0 with
                    | true -> return! loop balance.nzd_balance balance.btc_balance btcWithdraw
                    | false -> return! loop initialNzdBalance initialBtcBalance btcWithdraw
                | StoreWithdraw withdraw -> 
                    let total = btcWithdraw + withdraw
                    return! loop initialNzdBalance initialBtcBalance total
                | FetchBalance replyChannel -> 
                    replyChannel.Reply(initialNzdBalance, initialBtcBalance)
                    return! loop initialNzdBalance initialBtcBalance btcWithdraw
                | FetchWithdraw replyChannel -> 
                    replyChannel.Reply btcWithdraw
                    return! loop initialNzdBalance initialBtcBalance btcWithdraw
            }
        loop 0.0 0.0 0.0)

let private priceManager = 
    MailboxProcessor.Start(fun inbox -> 
        let rec loop previousPrice = 
            async { 
                let! msg = inbox.Receive()
                match msg with
                | FetchPrice replyChannel -> 
                    let exchangeRate = OpenExchangeRates.GetNzdBrlExchange()
                    let ticker = MercadoBitcoin.GetTicker()
                    let sellMinusFee = ticker.sell * (1.0 - totalFees)
                    
                    let brPrice = 
                        match ticker.buy < sellMinusFee with
                        | true -> ticker.buy
                        | false -> sellMinusFee
                    
                    let newPrice = Math.Round(brPrice / exchangeRate, 8)
                    match Math.Abs(newPrice - previousPrice) > 0.5 with
                    | true -> 
                        replyChannel.Reply(newPrice)
                        return! loop newPrice
                    | false -> 
                        replyChannel.Reply(previousPrice)
                        return! loop previousPrice
            }
        loop 0.0)

let IsFrenzyModeSet() = frenzyManager.PostAndReply(fun replyChannel -> IsFrenzyMode replyChannel)
let GetRecomendedPrice() = priceManager.PostAndReply(fun replyChannel -> FetchPrice replyChannel)
let GetInitialBalance() = balanceManager.PostAndReply(fun replyChannel -> FetchBalance replyChannel)
let GetWithdraw() = balanceManager.PostAndReply(fun replyChannel -> FetchWithdraw replyChannel)

let GetRecomendedAmount(balance : BitNZ.balance) = 
    let amount = Math.Round(balance.nzd_balance / (GetRecomendedPrice() * 2.0), 8)
    match amount < maxAmount with
    | true when amount < 0.5 -> amount * 2.0
    | true -> amount
    | false -> maxAmount

let DeleteOverpricedOrders (myOrders : seq<BitNZ.order>) maxPrice = 
    myOrders
    |> Seq.filter (fun x -> x.price > maxPrice)
    |> Seq.iter (fun x -> 
           printf "Deleting overpriced order"
           BitNZ.CancelOrder x)

let PrintStats(balance : BitNZ.balance) = 
    let initialBalance = GetInitialBalance()
    let avg = (balance.nzd_balance - fst initialBalance) / (balance.btc_balance + GetWithdraw() - snd initialBalance)
    printfn "NZD Balance  : %14.8f" balance.nzd_balance
    printfn "NZD Available: %14.8f" balance.nzd_available
    printfn "BTC Balance  : %14.8f" balance.btc_balance
    printfn "BTC Withdraw : %14.8f" (GetWithdraw())
    printfn "Avg BTC Price: %14.8f" avg
    printfn "Exchange rate: %14.8f" (OpenExchangeRates.GetNzdBrlExchange())
    printfn "Max bid price: %14.8f" (GetRecomendedPrice())
    printfn ""

let PrintOrders (myOrders : seq<BitNZ.order>) (buyOrderbook : seq<BitNZ.order>) = 
    buyOrderbook
    |> Seq.filter (fun x -> x.price >= (Seq.last myOrders).price)
    |> Seq.iter (fun x -> 
           printf "  %.8f  %.8f " x.price x.amount
           match myOrders |> Seq.tryFind (fun y -> y.price = x.price && y.amount = x.amount) with
           | None -> printfn ""
           | _ -> printfn "*")

let BuyBitcoins (myOrders : seq<BitNZ.order>) (sellOrderbook : seq<BitNZ.order>) (balance : BitNZ.balance) maxPrice = 
    sellOrderbook
    |> Seq.filter (fun x -> x.price < maxPrice)
    |> Seq.sortBy (fun x -> x.price)
    |> Seq.fold (fun (count, nzdAvailable, myOrdersList) sellOrder -> 
           let rec BuySellOrder count' nzdAvailable' myOrdersList' (sellOrder' : BitNZ.order) = 
               if (sellOrder'.price * sellOrder'.amount) <= nzdAvailable' then 
                   printfn "Buying from sell order"
                   BitNZ.CreateBuyOrder sellOrder'.price sellOrder'.amount
                   (count' + 1), (nzdAvailable' - (sellOrder'.price * sellOrder'.amount)), myOrdersList'
               else 
                   match myOrdersList' with
                   | head :: tail -> 
                       printfn "Canceling sell order"
                       BitNZ.CancelOrder head
                       let newAvailability = nzdAvailable' + (head.price * head.amount)
                       BuySellOrder count' newAvailability tail sellOrder'
                   | [] when nzdAvailable' > 10.0 -> 
                       let amount = Math.Round(nzdAvailable' / sellOrder'.price, 8) - 1e-8
                       printfn "Buying from sell order"
                       BitNZ.CreateBuyOrder sellOrder'.price amount
                       (count' + 1), (nzdAvailable' - (sellOrder'.price * amount)), myOrdersList'
                   | _ -> 
                       count', nzdAvailable', myOrdersList'
           
           BuySellOrder count nzdAvailable myOrdersList sellOrder) (0, balance.nzd_available, List.ofSeq myOrders)

let TestBuyBitcoins() =
    let balance : BitNZ.balance = { nzd_balance = 1020.0; btc_balance = 0.0; btc_available = 0.0; nzd_available = 170.0; nzd_reserved = 0.0; fee = ""; btc_reserved = 0.0 }
    let buyOrderbook, sellOrderbook = BitNZ.GetOrderbook()
    let m = seq<BitNZ.order> [ {id = 0; price = 430.0; amount = 1.0}; {id = 1; price = 420.0; amount = 1.0}; ]
    let count, _, _ = BuyBitcoins m sellOrderbook balance 440.0
    printfn "%A" count
    0


let Main() = 
    let balance = BitNZ.GetBalance()
    balanceManager.Post(StoreBalance balance)
    PrintStats balance
    let myOrders = BitNZ.GetBuyOrders()
    DeleteOverpricedOrders myOrders (GetRecomendedPrice())
    let buyOrderbook, sellOrderbook = BitNZ.GetOrderbook()
    let a = List.ofSeq buyOrderbook
    PrintOrders myOrders buyOrderbook
    let count, _, _ = BuyBitcoins myOrders sellOrderbook balance (GetRecomendedPrice())
    if count = 0 then
        ()

//    place_orders(my_orders, buy_orders, sell_orders, balance['nzd_available'], balance['nzd_balance'])
//    withdraw_btc(balance)
let rec Loop() = 
    printfn "=[ %s ]===================" (DateTime.Now.ToLongTimeString())
    try 
        TestBuyBitcoins() |> ignore
    with
    | :? System.Net.WebException as ex -> printfn "%A" ex
    | :? BitNZ.TransactionException as ex -> printfn "%A" ex
    match IsFrenzyModeSet() with
    | true -> Thread.Sleep(500)
    | false -> Thread.Sleep(sleepTime * 1000)
    Loop()