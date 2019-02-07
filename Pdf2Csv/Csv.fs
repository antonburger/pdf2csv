namespace Pdf2Csv

open System
open System.IO

module Csv =
    let formatMoney (amount: decimal) =
        amount.ToString("F2")

    let formatAmount = function
        | None -> ""
        | Some (Credit amount) -> formatMoney amount
        | Some (Debit amount) -> formatMoney -amount

    let write statement (writer: TextWriter) =
        let columns = [
            ("Date", fun tx -> tx.date.ToString("yyyy'-'MM'-'dd"))
            ("Description", fun tx -> tx.description)
            ("Amount", fun tx -> tx.amount |> formatAmount)
            ("Balance", fun tx -> tx.balance |> formatMoney)
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