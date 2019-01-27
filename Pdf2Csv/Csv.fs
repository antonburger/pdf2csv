namespace Pdf2Csv

open System
open System.IO

module Csv =
    let formatAmount (amount: decimal) =
        amount.ToString("F2")

    let formatCharge = function
        | None -> ""
        | Some (Cost (amount, flag)) -> sprintf "%s %A" (formatAmount amount) flag
        | Some Star -> "*"

    let formatDebit = function
        | Some (Debit amount) -> formatAmount amount
        | _ -> ""

    let formatCredit = function
        | Some (Credit amount) -> formatAmount amount
        | _ -> ""

    let write statement (writer: TextWriter) =
        let columns = [
            ("Date", fun tx -> tx.date.ToString("yyyy'-'MM'-'dd"))
            ("Description", fun tx -> tx.description)
            ("Charge", fun tx -> tx.charge |> formatCharge)
            ("Debit", fun tx -> tx.amount |> formatDebit)
            ("Credit", fun tx -> tx.amount |> formatCredit)
            ("Balance", fun tx -> tx.balance |> formatAmount)
        ]
        let escape (str: string) = "\"" + str.Replace("\"", "\"\"") + "\""
        let join (strs: seq<string>) = String.Join(",", strs)
        let header =
            columns
            |> List.map (fst >> escape)
            |> join
        let formatTx (tx: Transaction) =
            columns
            |> List.map (snd >> (fun f -> (f >> escape) tx))
            |> join
        writer.WriteLine header
        statement.transactions
        |> List.map formatTx
        |> List.iter writer.WriteLine