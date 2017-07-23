// Learn more about F# at http://fsharp.org
open System
open SkiaChart;

//Buy at alltime high and hold it for 5 years
//Buy at not all time high and hold it for 5 years
//Calculate and avg gain in both sets 

type HistoricalValue =
    {
        Close : Decimal
        Date : DateTime
    }

type Position =
    {
        Date: DateTime
        Price: Decimal
    }

type TransactionStarted = AllTimeHigh | NotAllTimeHigh

type TransactionResult = 
    {
        EnterPosition: Position
        ExitPosition: Position
        Type: TransactionStarted
    }

let calculateGain (result: TransactionResult) =     
    let gain = ((result.ExitPosition.Price / result.EnterPosition.Price)-1M)*100M
    gain

let printTransactionResult (result: TransactionResult) = 
    printfn "type: %s" (result.Type.ToString())
    printfn "Start: %s, %M" (result.EnterPosition.Date.ToShortDateString()) (result.EnterPosition.Price)
    printfn "end: %s, %M" (result.ExitPosition.Date.ToShortDateString()) (result.ExitPosition.Price)
    printfn "Gain: %M" (calculateGain result)
    printfn " "

//helper function that decides if the current value counts as alltime high 
//we interpret everything as alltime high which is either all time high OR 
//is within 3 trading days and 99% of the last all time high
let smoothAllTimeHighs (currentValue: decimal) (lastAllTimeHighValue: decimal) (tradingDaysSinceLastAllTimeHigh: int) (highestChangeFromAllTimeHigh: decimal) =
    let retVal = (currentValue >= lastAllTimeHighValue ||
                    (tradingDaysSinceLastAllTimeHigh < 3 && highestChangeFromAllTimeHigh > 99M ))
    retVal

let processCurrentValue (currentData: HistoricalValue) (allTimeHighPositions: List<Position>) (allTimeResults: List<TransactionResult>) (transactioStartType: TransactionStarted) = 
    let newPosition = { Date = currentData.Date; Price = currentData.Close }
    match allTimeHighPositions with 
     | [] -> allTimeHighPositions, allTimeResults
     | head::tail ->  
        match head with 
          | n when currentData.Date >= head.Date.AddYears(+5) ->
            let result = {TransactionResult.EnterPosition = head; TransactionResult.ExitPosition = newPosition; TransactionResult.Type = transactioStartType}
            (tail ), (allTimeResults @ [result]) 
          | _ -> allTimeHighPositions, allTimeResults

let decimalMax d1 d2 =
    if (d1>d2) then 
        d1
    else 
        d2

let increaseDaysSinceListAllTimeHigh currentValue currentAllTimeHighValue i =
    if currentValue < currentAllTimeHighValue then
        (i+1)
    else
        0
let ProcessData (historicalData: List<HistoricalValue>) = 
    let rec processDataIntern (historicalData: List<HistoricalValue>) (allTimeHigh: decimal) (allTimeHighPositions: List<Position>) 
        (notAllTimeHighPositions: List<Position>) (allTimeResults: List<TransactionResult>) 
            (notAllTimeResults: List<TransactionResult>) (daysSinceLastAllTimeHigh: int) (highestChangeSinceLastAllTimeHigh: decimal) =
        match historicalData with
            |  [ ] ->  allTimeResults, notAllTimeResults
            |  currentData::historicalDataTail ->
                    let newPosition = {Position.Date = currentData.Date; 
                                      Position.Price = currentData.Close}                   
                    let newAllTimeHighPositions, newAllTimeResults = processCurrentValue currentData allTimeHighPositions allTimeResults AllTimeHigh
                    let newNotAllTimeHighPositions, newNotAllTimeResults = processCurrentValue currentData notAllTimeHighPositions notAllTimeResults NotAllTimeHigh
                    if (smoothAllTimeHighs currentData.Close allTimeHigh daysSinceLastAllTimeHigh highestChangeSinceLastAllTimeHigh) then  //new all time high
                        let newAllTimeHigh = decimalMax currentData.Close allTimeHigh
                        processDataIntern historicalDataTail newAllTimeHigh 
                            (newAllTimeHighPositions @ [newPosition]) newNotAllTimeHighPositions newAllTimeResults 
                            newNotAllTimeResults (increaseDaysSinceListAllTimeHigh currentData.Close allTimeHigh daysSinceLastAllTimeHigh) 
                            ((newAllTimeHigh / allTimeHigh)*100M)                   
                    else
                        processDataIntern historicalDataTail allTimeHigh newAllTimeHighPositions 
                            (newNotAllTimeHighPositions @ [newPosition]) newAllTimeResults 
                            newNotAllTimeResults (daysSinceLastAllTimeHigh + 1) ((currentData.Close / allTimeHigh)*100M)

    processDataIntern historicalData historicalData.[0].Close [] [] [] [] 0 0M

let processLine (line: string) =
    let values = line.Split [|','|]
    {HistoricalValue.Close = Decimal.Parse(values.[4], System.Globalization.CultureInfo.InvariantCulture); HistoricalValue.Date = DateTime.Parse(values.[0]) }

let LoadHistoricalData (path: string)= 
    let lines = System.IO.File.ReadLines(path);
    let n = [];
    lines 
        |> Seq.skip 1
        |> Seq.filter (fun str -> not (str.Contains("null")))
        |> Seq.map(processLine)
        |> Seq.toList

let printDataInHistoricOrder (allTimeHighResults: list<TransactionResult>) (notAllTimeHighResults: list<TransactionResult>) =
    (allTimeHighResults @ notAllTimeHighResults) 
    |> List.sortBy(fun f -> f.EnterPosition.Date)
    |> List.iter(printTransactionResult)

let transformTypeToColor enterPositionType =
    match enterPositionType with
     | AllTimeHigh -> SkiaChart.PointColor.Green
     | NotAllTimeHigh -> SkiaChart.PointColor.Red

let createChart (allTimeHighTransactions: list<TransactionResult>) (notAllTimeHighTransactions: list<TransactionResult>) fileName =
    let width = 1200;
    let height = 900;
    let n = SkiaSharp.SKImageInfo(width, height);
    using(SkiaSharp.SKSurface.Create(n))  
                        (fun surface ->                                        
                                        let c = SkiaChart.SkiaChart(surface, width, height)               
                                        let data =  (allTimeHighTransactions @ notAllTimeHighTransactions) 
                                                    |> List.sortBy(fun f -> f.EnterPosition.Date)

                                        c.Dates <- data 
                                                    |> List.map(fun resultItem -> resultItem.EnterPosition.Date) 
                                                    |> List.toSeq

                                        c.Values <- data
                                                    |> List.map(fun resultItem -> 
                                                            Tuple.Create(resultItem.EnterPosition.Price, (transformTypeToColor resultItem.Type)))
                                                    |> List.toSeq
                                        c.Gains <- data
                                                    |> List.map(calculateGain)
                                                    |> List.toSeq
                                        c.RenderChart();                   
                        )    
    let img = SkiaSharp.SKImage.Create(n);
    use output = System.IO.File.OpenWrite(fileName + ".jpeg")
    img.Encode(SkiaSharp.SKEncodedImageFormat.Jpeg, 100).SaveTo(output);
    ()

let processDataForFile (fileName: string) = 
    let historicalData = LoadHistoricalData fileName
    printfn "processing %s" fileName 
    printfn "Historical Data Size: %i" (historicalData |> List.length)
    let processedData = ProcessData historicalData
    let allTimeHighTransactions, notAllTimeHighTransactions = processedData;
    createChart allTimeHighTransactions notAllTimeHighTransactions fileName

    //printDataInHistoricOrder allTimeHighTransactions notAllTimeHighTransactions
    printfn "number of finished transactions that were started at all time high: %i" (allTimeHighTransactions |> List.length)
    printfn "number of finished transactions that were startet at not at all time high: %i" (notAllTimeHighTransactions |> List.length)
    printfn "AllTimehigh avgGain: %M" (allTimeHighTransactions |> (List.map calculateGain) |> List.average)
    printfn "NOT AllTimehigh avgGain: %M" (notAllTimeHighTransactions |> (List.map calculateGain) |> List.average)
    printfn " "
    //SkiaChart.SkiaChart cc = new SkiaChart.SkiaChart();

let rec processDataForFiles (fileNames: list<string>) = 
    match fileNames with 
    | [] -> ()
    | head::tail -> processDataForFile head
                    processDataForFiles tail

[<EntryPoint>]
let main argv =
    processDataForFiles ["^DJI.csv"; "^GSPC.csv"; "^GDAXI.csv"]
    //processDataForFile "^DJI.csv"
    0