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

    let grayCharFrequency tiles = 
        tiles |> Seq.choose (function | (_,Gray(c)) -> Some c
                                      | _ -> None)
                                      |> Seq.countBy (fun ch->ch)
                                      |> Map.ofSeq

    let mutable grayFreq = grayCharFrequency tupleArray

    // For each gray input letter, check if in grayFreq and greater than 0
    // Reduce key value by one
    // Return yellow

    let tiles = tupleArray
                |> Array.map 
                 (function | (Gray(char), _) ->
                                match grayFreq.TryFind char with
                                | Some count when count > 0 ->
                                    grayFreq <- grayFreq.Change (char, Option.bind (fun k -> Some (k - 1)))
                                    Yellow(char)
                                | _ -> Gray(char)
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
        table.AddRow(row) |> ignore;
    AnsiConsole.Write(table)

let main word validWords maxGuesses =
    let rec loop (state: Tile[] list) = 
        if state.Length >= maxGuesses then
            AnsiConsole.MarkupLine $"Too bad, the answer was [white]{word}[/]"
            state
        else
            let input = AnsiConsole.Ask<string> "Please input your [gray]guess:[/]"

            match validate (Ok input) validWords with
            | Ok input ->
                let wordTile = toTiles input word
                if state |> List.contains wordTile then
                    AnsiConsole.MarkupLine $"[Red]Error you've already used {input} before![/]"
                    loop state 
                else
                    let state = state @ [wordTile]
                    printTiles state

                    if word = input then
                        AnsiConsole.WriteLine $"Well done you solved it in {state.Length} guesses"
                        state
                    else
                        loop state 

            | Error error ->
                AnsiConsole.MarkupLine $"[Red]{error}[/]"
                loop state 

    loop List.empty

let valLength = 5
let maxGuesses = 6
let validGuessText = File.ReadAllText("./ValidGuesses.txt")
let validWords = JsonConvert.DeserializeObject<string[]>(validGuessText)

let wordListText = File.ReadAllText("./WordList.txt")
let wordList = JsonConvert.DeserializeObject<string[]>(wordListText)

let mutable titleFig = FigletText("F# Wordle")
AnsiConsole.Write(titleFig);

let i = Random().Next(wordList.Length)
let target = wordList.[i];
main target validWords maxGuesses |> ignore