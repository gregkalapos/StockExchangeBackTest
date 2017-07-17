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
    let rec processDataIntern (historicalData: List<HistoricalValue>) (allTimeHigh: decimal) (allTimeHighPositions: seq<Position>) (notAllTimeHighPositions: seq<Position>) =
        let currentData = Seq.head historicalData
        let newAllTimePosition = {Position.EnterDate = currentData.Date; 
                                      Position.EnterPrice = currentData.Close}
        
        match historicalData with
            |  [ ] ->  2
            |  head::tail -> 
                    if currentData.Close > allTimeHigh then
                        processDataIntern (List.tail historicalData) currentData.Close allTimeHighPositions notAllTimeHighPositions
                    else 
                        processDataIntern (List.tail historicalData) currentData.Close allTimeHighPositions notAllTimeHighPositions
        
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

[<EntryPoint>]
let main argv =
    printfn "%A" (LoadHistoricalData "^DJI.csv")
    printfn "Hello World from F#!"
    0 // return an integer exit code