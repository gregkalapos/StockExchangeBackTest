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
        EnterDate: DateTime
        EnterPrice: Decimal
    }
    
let ProcessData (historicalData: List<HistoricalValue>) = 
    let rec processDataIntern (historicalData: List<HistoricalValue>) (allTimeHigh: decimal) (allTimeHighPositions: List<Position>) (notAllTimeHighPositions: List<Position>) =
        match historicalData with
            |  [ ] ->  allTimeHighPositions, notAllTimeHighPositions
            |  head::tail -> 
                    let currentData = Seq.head historicalData
                    let newPosition = {Position.EnterDate = currentData.Date; 
                                      Position.EnterPrice = currentData.Close}

                    if currentData.Close > allTimeHigh then
                        processDataIntern (List.tail historicalData) currentData.Close (allTimeHighPositions @ [newPosition]) notAllTimeHighPositions
                    else 
                        processDataIntern (List.tail historicalData) allTimeHigh allTimeHighPositions (notAllTimeHighPositions @ [newPosition])
        
    processDataIntern historicalData 0M [] []

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
    printfn "number of positions started at all time high: %i" (allTimeHighPositions |> List.length)
    printfn "number of positions started not at all time high: %i" (notAllTimeHighPositions |> List.length)
    0 // return an integer exit code