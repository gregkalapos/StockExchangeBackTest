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

let ProcessData (historicalData: List<HistoricalValue>) = 
    let rec processDataIntern (historicalData: List<HistoricalValue>) (allTimeHigh: decimal) (allTimeHighPositions: List<Position>) (notAllTimeHighPositions: List<Position>) (allTimeResults: List<TransactionResult>) (notAllTimeResults: List<TransactionResult>) =
        match historicalData with
            |  [ ] ->  allTimeResults, notAllTimeResults
            |  head::tail -> 
                    let currentData = Seq.head historicalData
                    let newPosition = {Position.Date = currentData.Date; 
                                      Position.Price = currentData.Close}
                    if currentData.Close > allTimeHigh then
                        let processFunction = processDataIntern (List.tail historicalData) currentData.Close (allTimeHighPositions @ [newPosition]) notAllTimeHighPositions
                        match allTimeHighPositions with
                            | [] -> processFunction allTimeResults notAllTimeResults
                            | head::tail ->  
                                match head with 
                                    | n when currentData.Date >= head.Date.AddYears(+5) ->
                                             let result = {TransactionResult.EnterPosition = head; TransactionResult.ExitPosition = newPosition}
                                             processFunction (allTimeResults @ [result]) notAllTimeResults
                                    | _ -> processFunction allTimeResults notAllTimeResults
                    else 
                        let processFunction = processDataIntern (List.tail historicalData) allTimeHigh allTimeHighPositions (notAllTimeHighPositions @ [newPosition])
                        match notAllTimeHighPositions with
                            | [] -> processFunction allTimeResults notAllTimeResults
                            | head::tail ->  
                                match head with 
                                    | n when currentData.Date >= head.Date.AddYears(+5) ->
                                             let result = {TransactionResult.EnterPosition = head; TransactionResult.ExitPosition = newPosition}
                                             processFunction allTimeResults (notAllTimeResults @ [result])
                                    | _ -> processFunction allTimeResults notAllTimeResults                        
        
    processDataIntern historicalData 0M [] [] [] []

let processLine (line: string) =
    let values = line.Split [|','|]
    {HistoricalValue.Close = Decimal.Parse(values.[4]); HistoricalValue.Date = DateTime.Parse(values.[0]) }

let LoadHistoricalData (path: string)= 
    let lines = System.IO.File.ReadLines(path);
    let n = [];
    lines 
        |> Seq.skip 1
        |> Seq.map(processLine)
        |> Seq.toList

[<EntryPoint>]
let main argv =
    let historicalData = LoadHistoricalData "^DJI.csv"
    let vv = ProcessData historicalData
    let allTimeHighPositions, notAllTimeHighPositions = vv;
    printfn "number of finished transactions that were started at all time high: %i" (allTimeHighPositions |> List.length)
    printfn "number of finished transactions that were startet at not at all time high: %i" (notAllTimeHighPositions |> List.length)

    0 // return an integer exit code