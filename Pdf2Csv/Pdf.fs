namespace Pdf2Csv

open System.IO
open System.Text
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Listener
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Geom

// http://cjhaas.com/blog/2013/03/13/itextsharp-slightly-smarter-text-extraction-strategy/

module Pdf =
    type LineComponent =
        | LineText of text: string * baseline: LineSegment
        | LineSpace

    type Line = Line of start: Vector * finish: Vector * pieces: LineComponent list

    let (|LineSegment|) (x: LineSegment) = x.GetStartPoint(), x.GetEndPoint()
    let (|Vector|) (x: Vector) = x.Get(0), x.Get(1)
    let (|LineStart|) (LineSegment (Vector (start, _), _)) = start

    type ReconstructPdfLinesFilter() =
        let findClosestLineTo
            (LineSegment (baselineStart, baselineEnd))
            (lines: Map<float32 * float32, Line>) =
            let baselineVector = baselineEnd.Subtract(baselineStart)
            let baselineNorm = baselineVector.LengthSquared()
            let toExistingLines =
                lines
                |> Seq.map (fun (KeyValue ((x, y), _)) -> let vec = Vector(x, y, 0.f) in (x, y), baselineVector.Cross(baselineEnd.Subtract(vec)).LengthSquared() / baselineNorm)
                |> Seq.sortBy snd
                |> Seq.truncate 1
                |> Seq.toList
            let distanceThreshold = 1.5f
            if List.isEmpty toExistingLines || (List.head >> snd) toExistingLines > distanceThreshold
                then None
            else
                let smallest = List.head toExistingLines
                if snd smallest > distanceThreshold
                    then None
                    else Some <| fst smallest

        let extendLine (renderInfo: TextRenderInfo, lines: Map<_, _>) =
            let (LineSegment (baselineStart, baselineEnd) as baseline) = renderInfo.GetBaseline()
            let existingLineKey = findClosestLineTo baseline lines
            let lineKey =
                defaultArg existingLineKey (let start = baselineStart in (start.Get(0), start.Get(1)))
            let newText = renderInfo.GetText();
            let newPiece = LineText (newText, baseline)
            let lineValue =
                lines.TryFind(lineKey)
                |> Option.map
                    (fun (Line (start, finish, (prev::_ as line))) ->
                        let previousWasSpace =
                            match prev with
                            | LineSpace -> true
                            | LineText (text, _) when text.[text.Length - 1] = ' ' -> true
                            | _ -> false
                        if not previousWasSpace && newText.Length > 0 && newText.[0] <> ' ' then
                            // we only insert a blank space if the trailing character of the previous string wasn't a space, and the leading character of the current string isn't a space
                            let spacing = finish.Subtract(baselineStart).Length()
                            if spacing > renderInfo.GetSingleSpaceWidth() / 2.f
                                then Line (start, baselineEnd, newPiece::LineSpace::line)
                                else Line (start, baselineEnd, newPiece::line)
                        else
                            Line (start, baselineEnd, newPiece::line))
            let lineValue = defaultArg lineValue <| Line (baselineStart, baselineEnd, [newPiece])
            lines.Add(lineKey, lineValue)

        member val Lines: Map<float32 * float32, Line> = Map.empty with get, set

        interface IEventListener with
            member this.GetSupportedEvents() =
                [| EventType.RENDER_TEXT |] :> _

            member this.EventOccurred(data, eventType) =
                if eventType.Equals(EventType.RENDER_TEXT) then
                    this.Lines <- extendLine(data :?> TextRenderInfo, this.Lines)

    type FilterState =
        | OutOfTransaction of Transaction list
        | InTransaction of Columns * Transaction list
    and Columns = {
        date: Span
        description: Span
        charge: Span
        debit: Span
        credit: Span
        balance: Span
    }
    and Span = float32 * float32
    and TransactionLine = {
        dateText: string
        descriptionText: string
        chargeText: string
        debitText: string
        creditText: string
        balanceText: string
    }

    let render (col: Span) line =
        let isContained spacesAreContained lineComponent =
            match lineComponent with
            | LineSpace -> spacesAreContained
            | LineText (_, LineStart start) ->
                fst col <= start && start <= snd col
        let sb =
            line
            |> Seq.skipWhile (isContained false >> not)
            |> Seq.takeWhile (isContained true)
            |> Seq.fold (fun (sb: StringBuilder) c ->
                let t =
                    match c with
                    | LineSpace -> " "
                    | LineText (t, _) -> t
                sb.Append(t)) (StringBuilder())
        sb.ToString()

    let rec lineStartsAt line =
        match line with
        | [] -> None
        | c::cs ->
            match c with
            | LineSpace -> lineStartsAt cs
            | LineText (_, LineStart start) -> Some start

    let (|TxDate|_|) (dateText : string) =
        let formats = [|
                "d'/'M'/'yyyy"
                "dd'/'M'/'yyyy"
                "d'/'MM'/'yyyy"
                "dd'/'MM'/'yyyy"
            |]
        match System.DateTime.TryParseExact(dateText, formats, null, System.Globalization.DateTimeStyles.AllowTrailingWhite) with
        | true, date -> Some date
        | false, _ -> None

    let parseCharge (ch: string) =
        let ch = ch.Trim()
        if (System.String.IsNullOrWhiteSpace(ch)) then None
        elif ch.EndsWith("*") then Some Star
        else
            let ch, flag =
                if ch.EndsWith("T") then ch.Substring(0, ch.Length - 1).Trim(), T
                else ch, NoFlag
            let ch = ch.Replace(" ", "")
            match System.Decimal.TryParse(ch, System.Globalization.NumberStyles.AllowDecimalPoint, null) with
            | true, amount -> Cost (amount, flag) |> Some
            | false, _ -> None

    let parseAmount debit credit =
        let debitIsBlank = System.String.IsNullOrWhiteSpace(debit)
        let creditIsBlank = System.String.IsNullOrWhiteSpace(credit)
        if debitIsBlank && creditIsBlank then None
        elif not debitIsBlank && not creditIsBlank then failwith "Both debit and credit!"
        else
            let amount = if debitIsBlank then credit else debit
            let amount = amount.Replace(" ", "")
            let amount = System.Decimal.Parse(amount, System.Globalization.NumberStyles.AllowDecimalPoint, null)
            if debitIsBlank then Credit amount |> Some else Debit amount |> Some

    let parseBalance (amount: string) =
        let amount = amount.Replace(" ", "")
        System.Decimal.Parse(amount, System.Globalization.NumberStyles.AllowDecimalPoint ||| System.Globalization.NumberStyles.AllowTrailingSign, null)

    let (|TxHeader|_|) cols (Line (_, _, line)) =
        let dateText = render cols.date line
        match dateText with
        | TxDate date ->
            Some {
                Transaction.date = date
                description = render cols.description line
                charge = render cols.charge line |> parseCharge
                amount = parseAmount (render cols.debit line) (render cols.credit line)
                balance = render cols.balance line |> parseBalance
            }
        | _ -> None

    let (|TxDescription|_|) cols (Line (Vector (start, _), _, line)) =
        if fst cols.description <= start && start <= snd cols.description
            then Some <| render cols.description line
            else None

    let reducer state (Line (start, _, line) as l) =
        match state with
        | OutOfTransaction ts ->
            match line |> List.filter (function | LineText _ -> true | _ -> false) with
            | (LineText ("Date", LineStart dateStart))::
              (LineText ("Transaction Description", LineStart descriptionStart))::
              (LineText ("Charge", LineStart chargeStart))::
              (LineText ("Debit Amount", LineStart debitStart))::
              (LineText ("Credit Amount", LineStart creditStart))::
              (LineText ("Balance", LineStart balanceStart))::
              _ ->
                  let truncate f =
                      let digits = 2
                      let factor = System.Math.Pow(10., float digits)
                      System.Math.Truncate(float f * factor) / factor |> float32
                  let dateStart = truncate dateStart
                  let descriptionStart = truncate descriptionStart
                  let chargeStart = truncate chargeStart
                  let debitStart = truncate debitStart
                  let creditStart = truncate creditStart
                  let balanceStart = truncate (balanceStart - 10.f)
                  InTransaction ({
                      date = dateStart, descriptionStart
                      description = descriptionStart, chargeStart
                      charge = chargeStart, debitStart
                      debit = debitStart, creditStart
                      credit = creditStart, balanceStart
                      balance = balanceStart, balanceStart + 1000.f
                  }, ts)
            | _ -> state
        | InTransaction (cols, ts) ->
            match l, ts with
            | TxHeader cols tx, _ -> InTransaction (cols, tx::ts)
            | TxDescription cols desc, tx::ts ->
                let description = tx.description + " " + desc
                InTransaction (cols, { tx with description = description }::ts)
            | TxDescription cols _, [] -> failwith "Found transaction description before header!"
            | _ -> OutOfTransaction ts

    let load (s: Stream) (password: byte[]) =
        let mutable pr = ReaderProperties()
        if password.Length > 0 then
            pr <- ReaderProperties().SetPassword(password)
        use reader = new PdfReader(s, pr)
        use doc = new PdfDocument(reader)
        let pages = [1 .. doc.GetNumberOfPages()] |> List.map (fun p -> doc.GetPage(p))
        let filters = pages |> List.map (fun _ -> ReconstructPdfLinesFilter())
        let processors =
            filters
            |> List.map PdfCanvasProcessor
        List.zip processors pages
        |> List.iter (fun (processor, page) -> processor.ProcessPageContent(page))
        let lines =
            filters
            |> Seq.collect (fun f -> f.Lines |> Seq.sortBy (fun (KeyValue ((_, y), _)) -> -y) |> Seq.map (fun (KeyValue (_, (Line (start, finish, line)))) -> Line (start, finish, List.rev line)))
            |> Seq.toList

        let finalState =
            lines
            |> List.fold reducer (OutOfTransaction [])

        match finalState with
        | InTransaction _ -> failwith "Terminated inside transaction list unexpectedly"
        | OutOfTransaction ts ->
            List.rev ts

