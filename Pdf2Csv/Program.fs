// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

namespace Pdf2Csv

open System
open System.IO

module Program =
    let run path password =
        let printVerify label amount =
            let original = Console.ForegroundColor
            try
                Console.ForegroundColor <- ConsoleColor.DarkYellow
                printf "VERIFY: "
                Console.ForegroundColor <- original
                printf "Check that the "
                Console.ForegroundColor <- ConsoleColor.Yellow
                printf "%s " label
                Console.ForegroundColor <- original
                printf "is "
                Console.ForegroundColor <- ConsoleColor.Yellow
                printfn "%.2f " amount
            finally
                Console.ForegroundColor <- original
        try
            let password = System.Text.Encoding.ASCII.GetBytes(s = password)
            use pdf = File.OpenRead(path)
            let statement =
                Pdf.load pdf password
                |> Statement.create
            match statement with
            | Ok statement ->
                printVerify "opening balance" statement.openingBalance
                printVerify "closing balance" statement.closingBalance
                printfn ""
                printfn "Found %i transactions" (statement.transactions |> List.length)
                let path = Path.ChangeExtension(path, ".csv") |> Path.GetFullPath
                use file = new FileStream(path, FileMode.Create, FileAccess.ReadWrite)
                use writer = new StreamWriter(file)
                Csv.write statement (writer)
                printfn "Wrote output file '%s'" path
            | Error errors ->
                printfn "ERRORS when trying to parse the statement:"
                List.iter (printfn "%s") errors
        with
        | e -> printfn "%s" (e.Message)

    [<EntryPoint>]
    let main argv = 
        match argv |> Array.toList with
        | path::password::_ -> run path password
        | path::_ -> run path ""
        | _ -> printfn "Expecting path to statement and password"
        0
