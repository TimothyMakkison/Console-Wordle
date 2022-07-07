open Newtonsoft.Json
open System.IO
open Spectre.Console
open System

exception UnequalLengths of string * string

type Tile =
    | Green of character : char
    | Yellow of character : char
    | Gray of character : char

let validateLength (value: string) =
    match value.Length with
    | 5 -> Ok value
    | _ ->  Error "Error: input must be 5 characters long"

let isValidWord validWords value  =
    let index  = System.Array.BinarySearch(validWords, value)
    match index with
       | (i) when i >=0 -> Ok value
       | _ ->  Error "Error: not a valid word"

let validate value validWords =
    value
    |> Result.bind validateLength
    |> Result.bind (isValidWord validWords)

let toTiles (input:string) (expected:string) =
    if input.Length <> expected.Length then
        raise (UnequalLengths(input, expected))

    // Green if characters match else gray
    let toGreenOrGray l r =
        match (l, r) with
        | (l, r) when l = r -> (Green(l),Green(r))
        | _ -> (Gray(l),Gray(r))

    let tupleArray =
        (input, expected)
        ||> Seq.map2 toGreenOrGray
        |> Seq.toArray

    let tryFindExisting targetChar arr =
        arr |> Array.tryFindIndex (fun (_,r) -> Gray(targetChar) = r)

    let tiles = tupleArray
                |> Array.map
                        (fun pair ->
                            match pair with
                            | (Gray(char), _) ->
                                match tryFindExisting char tupleArray with
                                | Some (index) ->
                                    let (l,_) = tupleArray.[index]
                                    tupleArray.[index] <- (l,Yellow(' '))
                                    Yellow(char)
                                | None -> Gray(char)
                            | (l,_) -> l)

    tiles

let printTiles (tiles: Tile[] list) =
    let empty = Array.zeroCreate<string> 5 |> Array.map (fun _ -> "")
    let mutable table = Table().HeavyBorder().AddColumns(empty)
    table.ShowHeaders <- false
    let toUpper c = System.Char.ToUpper c

    let prettyString tile =
        match tile with
        | Green(c) -> $"[white on green] {toUpper c} [/]"
        | Yellow(c) -> $"[white on darkorange3] {toUpper c} [/]"
        | Gray(c) -> $"[white on gray] {toUpper c} [/]"

    for tile in tiles do
        let row = tile |> Array.map prettyString
        table = table.AddRow(row) |> ignore;
    AnsiConsole.Write(table)

let rec main word (state: Tile[] list) validWords =
    if state.Length >= 6 then
        AnsiConsole.MarkupLine $"Too bad, the answer was [white]{word}[/]"
        state
    else
        let line = AnsiConsole.Ask<string> "Please input your [gray]guess:[/]"

        match validate (Ok line) validWords with
        | Ok input ->
            let wordTile = toTiles input word
            if state |> List.contains wordTile then
                AnsiConsole.MarkupLine $"[Red]Error you've already used {input} before![/]"
                main word state validWords
            else
                let state = state @ [wordTile]
                printTiles state

                if word = input then
                    AnsiConsole.WriteLine $"Well done you solved it in {state.Length} guesses"
                    state
                else
                    main word state validWords

        | Error error ->
            AnsiConsole.MarkupLine $"[Red]{error}[/]"
            main word state validWords

let valLength = 5
let validGuessText = File.ReadAllText("./ValidGuesses.txt")
let validWords = JsonConvert.DeserializeObject<string[]>(validGuessText)

let wordListText = File.ReadAllText("./WordList.txt")
let wordList = JsonConvert.DeserializeObject<string[]>(wordListText)

let mutable titleFig = FigletText("F# Wordle")
AnsiConsole.Write(titleFig);

let i = Random().Next(wordList.Length)
let wordGoal = wordList.[i];
main wordGoal List.empty validWords |> ignore