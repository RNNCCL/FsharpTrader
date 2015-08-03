module TraderBot

open Utils
open System
open System.Threading

let private totalFees = LoadAppSetting<decimal> "totalFees"
let private maxAmount = LoadAppSetting<decimal> "maxAmount"
let private gapPriceForReduce = LoadAppSetting<decimal> "gapPriceForReduce"
let private gapAmountForReplace = LoadAppSetting<decimal> "gapAmountForReplace"
let private frenzyTime = LoadAppSetting<float> "frenzyTime"
let private sleepTime = LoadAppSetting<int> "sleepTime"

type private msg1 = 
    | SetFrenzyMode
    | IsFrenzyMode of AsyncReplyChannel<bool>

type private msg2 = 
    | StoreBalance of BitNZ.balance
    | StoreWithdraw of decimal
    | FetchBalance of AsyncReplyChannel<decimal * decimal>
    | FetchWithdraw of AsyncReplyChannel<decimal>

type private msg3 = 
    | FetchPrice of AsyncReplyChannel<decimal>

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
                    match initialNzdBalance = 0.0m && initialBtcBalance = 0.0m with
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
        loop 0.0m 0.0m 0.0m)

let private priceManager = 
    MailboxProcessor.Start(fun inbox -> 
        let rec loop previousPrice = 
            async { 
                let! msg = inbox.Receive()
                match msg with
                | FetchPrice replyChannel -> 
                    let exchangeRate = OpenExchangeRates.GetNzdBrlExchange()
                    let ticker = MercadoBitcoin.GetTicker()
                    let sellMinusFee = ticker.sell * (1.0m - totalFees)
                    
                    let brPrice = 
                        match ticker.buy < sellMinusFee with
                        | true -> ticker.buy
                        | false -> sellMinusFee
                    
                    let newPrice = brPrice / exchangeRate
                    match Math.Abs(newPrice - previousPrice) > 0.5m with
                    | true -> 
                        replyChannel.Reply(newPrice)
                        return! loop newPrice
                    | false -> 
                        replyChannel.Reply(previousPrice)
                        return! loop previousPrice
            }
        loop 0.0m)

let SetFrenzyMode() = frenzyManager.Post SetFrenzyMode
let IsFrenzyModeSet() = frenzyManager.PostAndReply(fun replyChannel -> IsFrenzyMode replyChannel)
let GetRecomendedPrice() = priceManager.PostAndReply(fun replyChannel -> FetchPrice replyChannel)
let GetInitialBalance() = balanceManager.PostAndReply(fun replyChannel -> FetchBalance replyChannel)
let GetWithdraw() = balanceManager.PostAndReply(fun replyChannel -> FetchWithdraw replyChannel)

let PrintStats(balance : BitNZ.balance) = 
    let initialBalance = GetInitialBalance()
    //let avg = (balance.nzd_balance - fst initialBalance) / (balance.btc_balance + GetWithdraw() - snd initialBalance)
    TerminalDispatcher.PrintBalanceData "NZD Balance" balance.nzd_balance
    TerminalDispatcher.PrintBalanceData "NZD Available" balance.btc_balance
    TerminalDispatcher.PrintBalanceData "BTC Balance" (GetWithdraw())
    TerminalDispatcher.PrintBalanceData "BTC Withdraw" 0m
    TerminalDispatcher.PrintBalanceData "Avg BTC Price" 0m
    TerminalDispatcher.PrintBalanceData "Exchange rate" (OpenExchangeRates.GetNzdBrlExchange())
    TerminalDispatcher.PrintBalanceData "Max bid price" (GetRecomendedPrice())
    printfn ""

let PrintOrders (myOrders : seq<BitNZ.order>) (buyOrderbook : seq<BitNZ.order>) = 
    let firstOrder = 
        myOrders
        |> Seq.sortBy (fun x -> x.price)
        |> Seq.tryHead
    match firstOrder with
    | Some order -> 
        buyOrderbook
        |> Seq.filter (fun x -> x.price >= order.price)
        |> Seq.iter (fun x -> 
               let owner =
                   match myOrders |> Seq.tryFind (fun y -> y.price = x.price) with
                   | None -> TerminalDispatcher.OrderOwner.ThirdPart
                   | Some order when order.amount = x.amount -> TerminalDispatcher.OrderOwner.Me
                   | _ -> TerminalDispatcher.OrderOwner.Group
               TerminalDispatcher.PrintOrderList x.price x.amount owner)
    | None -> ()

let DeleteOverpricedOrders (myOrders : seq<BitNZ.order>) nzdAvailable maxPrice = 
    myOrders |> Seq.fold (fun (nzdAvailable', myOrdersList) x -> 
                    if x.price > maxPrice then 
                        BitNZ.CancelOrder x "OV"
                        nzdAvailable' + (x.price * x.amount), myOrdersList
                    else nzdAvailable', (x :: myOrdersList)) (nzdAvailable, [])

let BuyFromSellingOrders (sellOrderbook : seq<BitNZ.order>) (myOrders : seq<BitNZ.order>) nzdAvailable maxPrice = 
    let myOrdersList = 
        myOrders
        |> Seq.sortBy (fun x -> x.price)
        |> List.ofSeq
    sellOrderbook
    |> Seq.filter (fun x -> x.price < maxPrice)
    |> Seq.sortBy (fun x -> x.price)
    |> Seq.fold (fun (count1, nzdAvailable1, myOrdersList1) sellOrder -> 
           let rec BuySellOrder count2 nzdAvailable2 myOrdersList2 (sellOrder1 : BitNZ.order) = 
               if (sellOrder1.price * sellOrder1.amount) <= nzdAvailable2 then 
                   BitNZ.CreateBuyOrder sellOrder1.price sellOrder1.amount "S0" |> ignore
                   (count2 + 1), (nzdAvailable2 - (sellOrder1.price * sellOrder1.amount)), myOrdersList2
               else 
                   match myOrdersList2 with
                   | head :: tail -> 
                       BitNZ.CancelOrder head "S1"
                       let newAvailability = nzdAvailable2 + (head.price * head.amount)
                       BuySellOrder count2 newAvailability tail sellOrder1
                   | [] when nzdAvailable2 > 2m -> 
                       let amount = (nzdAvailable2 / sellOrder1.price) - 1e-8m
                       BitNZ.CreateBuyOrder sellOrder1.price amount "S2" |> ignore
                       (count2 + 1), (nzdAvailable2 - (sellOrder1.price * amount)), myOrdersList2
                   | _ -> count2, nzdAvailable2, myOrdersList2
           BuySellOrder count1 nzdAvailable1 myOrdersList1 sellOrder) (0, nzdAvailable, myOrdersList)

let GetRecomendedAmount nzdAvailable price = 
    let a1 = (nzdAvailable - 0.1m) / (price * 2m)
    match a1 > maxAmount with
    | false when a1 < 0.5m -> a1 * 2m
    | false -> a1
    | true -> maxAmount

let ReplaceOrder oldOrder newPrice newAmount nzdAvailable tag = 
    if newPrice * newAmount < nzdAvailable then 
        BitNZ.CreateBuyOrder newPrice newAmount tag |> ignore
        BitNZ.CancelOrder oldOrder tag
    else 
        BitNZ.CancelOrder oldOrder tag
        BitNZ.CreateBuyOrder newPrice newAmount tag |> ignore

let PlaceAndAdjustOrders (buyOrderbook : seq<BitNZ.order>) (myOrders : BitNZ.order list) nzdAvailable maxPrice = 
    let competingOrder = buyOrderbook |> Seq.filter (fun x -> x.price < maxPrice)
                                      |> Seq.head
    
    let price = competingOrder.price + 1e-8m
    match myOrders |> List.sortByDescending (fun x -> x.price)
                   |> List.tryHead 
                   with
    | None -> 
        let amount = GetRecomendedAmount nzdAvailable maxPrice
        BitNZ.CreateBuyOrder price amount "CO" |> ignore
    | Some wannaBeTopOrder when wannaBeTopOrder.price <> competingOrder.price || wannaBeTopOrder.amount <> competingOrder.amount -> 
        let totalAvailable = nzdAvailable + (wannaBeTopOrder.price * wannaBeTopOrder.amount)
        let amount = GetRecomendedAmount totalAvailable maxPrice
        ReplaceOrder wannaBeTopOrder price amount nzdAvailable "GT"
        SetFrenzyMode()
    | Some topOrder -> 
        let totalAvailable = nzdAvailable + (topOrder.price * topOrder.amount)
        let amount = GetRecomendedAmount totalAvailable maxPrice
        let previousOrder = buyOrderbook |> Seq.filter (fun x -> x.price < topOrder.price)            
                                         |> Seq.head
        if topOrder.price - previousOrder.price > gapPriceForReduce || amount - topOrder.amount > gapAmountForReplace then 
            ReplaceOrder topOrder (previousOrder.price + 1e-8m) amount nzdAvailable "AO"


let TestBuyBitcoins() = 

    let balance2 = BitNZ.GetBalance()
    PrintStats balance2

    let balance : BitNZ.balance = 
        { nzd_balance = 20.0m
          btc_balance = 0.0m
          btc_available = 0.0m
          nzd_available = 100.0m
          nzd_reserved = 0.0m
          btc_reserved = 0.0m }
    
    let maxPrice = 435.0m
    
    let myOrders = 
        seq<BitNZ.order> [ 
                           { id = 4
                             price = 432.8m
                             amount = 1.04986796m }
                           { id = 3
                             price = 431.96m
                             amount = 0.1m }
                           { id = 6
                             price = 430.46000001m
                             amount = 0.21m }
                           { id = 5
                             price = 430.0m
                             amount = 0.65m }
                           { id = 1
                             price = 440.35m
                             amount = 0.3858m }
                           { id = 7
                             price = 427.201m
                             amount = 0.0251m } ]
    
    //let myOrders = BitNZ.GetBuyOrders()
    let buyOrderbook, sellOrderbook = BitNZ.GetOrderbook()
    let available1, myOrders1 = DeleteOverpricedOrders myOrders balance.nzd_available maxPrice
    PrintOrders myOrders buyOrderbook
    let _, available2, myOrders2 = BuyFromSellingOrders sellOrderbook myOrders1 available1 maxPrice
    PlaceAndAdjustOrders buyOrderbook myOrders2 available2 maxPrice
//    printfn "%A" 12
    0

let Main() = 
    let balance = BitNZ.GetBalance()
    0

//    balanceManager.Post(StoreBalance balance)
//    PrintStats balance
//    let myOrders = BitNZ.GetBuyOrders()
//    //DeleteOverpricedOrders myOrders (GetRecomendedPrice())
//    let buyOrderbook, sellOrderbook = BitNZ.GetOrderbook()
//    let a = List.ofSeq buyOrderbook
//    PrintOrders myOrders buyOrderbook
//    let count, _, _ = BuyBitcoins myOrders sellOrderbook balance (GetRecomendedPrice())
//    if count = 0 then ()
//    place_orders(my_orders, buy_orders, sell_orders, balance['nzd_available'], balance['nzd_balance'])
//    withdraw_btc(balance)
let rec Loop() = 
    printfn "=[ %s ]====================" (DateTime.Now.ToString("HH:mm:ss"))
    try 
        TestBuyBitcoins() |> ignore
    with
    | :? System.Net.WebException as ex -> printfn "%A" ex
    | :? BitNZ.TransactionException as ex -> printfn "%A" ex
    match IsFrenzyModeSet() with
    | true -> 
        printfn "\n- Frenzy mode set! -\n"
        Thread.Sleep(500)
    | false -> Thread.Sleep(sleepTime * 1000)
    Loop()
