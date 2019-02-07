namespace Pdf2Csv

open System

type Statement = {
    openingBalance: decimal
    closingBalance: decimal
    transactions: Transaction list
}

and Transaction = {
    date: DateTime
    description: string
    charge: Charge option
    amount: Amount option
    balance: decimal
}
and Charge =
    | Cost of decimal * Flag
    | Star
and Flag = NoFlag | T
and Amount =
    | Credit of decimal
    | Debit of decimal

type Result<'R, 'E> =
    | Ok of 'R
    | Error of 'E

module Statement =
    let create ts =
        let rec reconcile state tx =
            match state with
            | Ok balance ->
                let expected = tx.balance
                let actual =
                    match tx.amount with
                    | None -> balance
                    | Some (Credit amount) -> balance + amount
                    | Some (Debit amount) -> balance - amount
                if expected = actual
                then Ok expected
                else
                    let operation =
                        match tx.amount with
                        | None -> "no change"
                        | Some (Credit amount) -> sprintf "a credit of %.2M" amount
                        | Some (Debit amount) -> sprintf "a debit of %.2M" amount
                    Error <| [sprintf "Applying %s to running balance of %.2M produces %.2M instead of expected balance of %.2M" operation balance actual expected]
            | _ -> state
        
        if List.isEmpty ts then
            Error ["No transactions identified"]
        else
            let firstTx = List.head ts
            match List.fold reconcile (Ok firstTx.balance) ts with
            | Error es -> Error es
            | Ok balance ->
                Ok <| {
                    openingBalance = firstTx.balance
                    closingBalance = balance
                    transactions = ts
                }
            
