// Learn more about F# at http://fsharp.org

open System

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
  
type TransactionResult = 
    {
        EnterPosition: Position
        ExitPosition: Position
    }

let calculateGain (result: TransactionResult) (str: string) =     
    let gain = ((result.ExitPosition.Price / result.EnterPosition.Price)-1M)*100M
    printf "type: %s" str
    printfn "Start: %s, %M" (result.EnterPosition.Date.ToShortDateString()) (result.EnterPosition.Price)
    printfn "end: %s, %M" (result.ExitPosition.Date.ToShortDateString()) (result.ExitPosition.Price)
    printfn "Gain: %M" gain
    printfn " "
    gain


//TODO rename
let processAllTimeHighs (currentData: HistoricalValue) (allTimeHighPositions: List<Position>) (allTimeResults: List<TransactionResult>) = 
    let newPosition = { Date = currentData.Date; Price = currentData.Close }
    match allTimeHighPositions with 
     | [] -> allTimeHighPositions, allTimeResults
     | head::tail ->  
        match head with 
          | n when currentData.Date >= head.Date.AddYears(+5) ->
            let result = {TransactionResult.EnterPosition = head; TransactionResult.ExitPosition = newPosition}
            (tail @  [newPosition]), (allTimeResults @ [result]) 
          | _ -> allTimeHighPositions, allTimeResults

// let procesNotAllTimeHighs (currentData: HistoricalValue) (notAllTimeHighPositions: List<Position>) (notAllTimeResults: List<TransactionResult>) =
//     let newPosition = { Date = currentData.Date; Price = currentData.Close } 
//     match notAllTimeHighPositions with
//       | [] -> notAllTimeHighPositions, notAllTimeResults
//       | head::tail ->  
//          match head with 
//            | n when currentData.Date >= head.Date.AddYears(+5) ->
//              let result = {TransactionResult.EnterPosition = head; TransactionResult.ExitPosition = newPosition}
//              (tail @ [newPosition]), (notAllTimeResults @ [result])
//            | _ -> notAllTimeHighPositions, notAllTimeResults


let ProcessData (historicalData: List<HistoricalValue>) = 
    let rec processDataIntern (historicalData: List<HistoricalValue>) (allTimeHigh: decimal) (allTimeHighPositions: List<Position>) (notAllTimeHighPositions: List<Position>) (allTimeResults: List<TransactionResult>) (notAllTimeResults: List<TransactionResult>) =
        match historicalData with
            |  [ ] ->  allTimeResults, notAllTimeResults
            |  currentData::historicalDataTail ->                     
                   
                    let newPosition = {Position.Date = currentData.Date; 
                                      Position.Price = currentData.Close}

                    let processFunction = processDataIntern historicalDataTail
                    let newAllTimeHighPositions, newAllTimeResults = processAllTimeHighs currentData allTimeHighPositions allTimeResults
                    let newNotAllTimeHighPositions, newNotAllTimeResults = processAllTimeHighs currentData notAllTimeHighPositions notAllTimeResults
                                      
                    if currentData.Close > allTimeHigh then           
                        processDataIntern historicalDataTail currentData.Close newAllTimeHighPositions newNotAllTimeHighPositions newAllTimeResults newNotAllTimeResults
                        // let processFunction = processDataIntern historicalDataTail currentData.Close 
                        // match allTimeHighPositions with
                        //     | [] -> processFunction (allTimeHighPositions @ [newPosition]) notAllTimeHighPositions allTimeResults notAllTimeResults
                        //     | head::tail ->  
                        //         match head with 
                        //             | n when currentData.Date >= head.Date.AddYears(+5) ->
                        //                      let result = {TransactionResult.EnterPosition = head; TransactionResult.ExitPosition = newPosition}
                        //                      processFunction (tail @  [newPosition]) notAllTimeHighPositions (allTimeResults @ [result]) notAllTimeResults
                        //             | _ -> processFunction (allTimeHighPositions @ [newPosition]) notAllTimeHighPositions allTimeResults notAllTimeResults
                    else 
                        processDataIntern historicalDataTail allTimeHigh newAllTimeHighPositions newNotAllTimeHighPositions newAllTimeResults newNotAllTimeResults
                        // let processFunction = processDataIntern historicalDataTail allTimeHigh allTimeHighPositions 
                        // match notAllTimeHighPositions with
                        //     | [] -> processFunction (notAllTimeHighPositions @ [newPosition]) allTimeResults notAllTimeResults
                        //     | head::tail ->  
                        //         match head with 
                        //             | n when currentData.Date >= head.Date.AddYears(+5) ->
                        //                      let result = {TransactionResult.EnterPosition = head; TransactionResult.ExitPosition = newPosition}
                        //                      processFunction (tail @ [newPosition]) allTimeResults (notAllTimeResults @ [result])
                        //             | _ -> processFunction (notAllTimeHighPositions @ [newPosition]) allTimeResults notAllTimeResults                        
        
    processDataIntern historicalData 0M [] [] [] []

let processLine (line: string) =
    let values = line.Split [|','|]
    {HistoricalValue.Close = Decimal.Parse(values.[4], System.Globalization.CultureInfo.InvariantCulture); HistoricalValue.Date = DateTime.Parse(values.[0]) }

let LoadHistoricalData (path: string)= 
    let lines = System.IO.File.ReadLines(path);
    let n = [];
    lines 
        |> Seq.skip 1
        |> Seq.map(processLine)
        |> Seq.toList

let callForAH (result: TransactionResult)=
    calculateGain result "AllTimeHigh"

let callForNOTAH (result: TransactionResult)=
    calculateGain result "callForNOTAH"

[<EntryPoint>]
let main argv =
    let historicalData = LoadHistoricalData "^DJI.csv"
    let vv = ProcessData historicalData
    let allTimeHighPositions, notAllTimeHighPositions = vv;
    printfn "number of finished transactions that were started at all time high: %i" (allTimeHighPositions |> List.length)
    printfn "number of finished transactions that were startet at not at all time high: %i" (notAllTimeHighPositions |> List.length)

    printfn "AllTimehigh avgGain: %M" (allTimeHighPositions |> (List.map callForAH) |> List.average)
    printfn "NOT AllTimehigh avgGain: %M" (notAllTimeHighPositions |> (List.map callForNOTAH) |> List.average)

    0 // return an integer exit code