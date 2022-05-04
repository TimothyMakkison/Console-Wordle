open Newtonsoft.Json
open System.IO
open Spectre.Console
open System

exception UnequalLengths of string * string

type Tile =
    | Green of character : char
    | Yellow of character : char
    | Gray of character : char

let valLength = 5
let validGuessText = File.ReadAllText("./ValidGuesses.txt")
let validWords = JsonConvert.DeserializeObject<string[]>(validGuessText)

let wordListText = File.ReadAllText("./WordList.txt")
let wordList = JsonConvert.DeserializeObject<string[]>(wordListText)

let validateLength (value: string) =
    match value.Length with
    | 5 -> Ok value
    | _ ->  Error "Error: input must be 5 characters long"

let isValidWord value validWords =
    let index  = System.Array.BinarySearch(validWords, value)
    match index with
       | (i) when i >=0 -> Ok value
       | _ ->  Error "Error: not a valid word"

let validate value validWords =
    value 
    |> Result.bind validateLength 
    |> Result.bind (fun v -> isValidWord v validWords)

let toTiles (input:string) (expected:string) =
    if input.Length <> expected.Length then 
        raise (UnequalLengths(input, expected))

    let tupleArray = 
        (input, expected) ||> Seq.map2 (
            fun i e-> match (i, e) with
                      | (i, e) when i = e -> (Green(i),Green(e))
                      | _ -> (Gray(i),Gray(e)))
                      |> Seq.toArray

    let tryFindExisting targetChar arr = 
        arr 
        |> Array.indexed 
        |> Array.tryFind 
            (fun pair -> match pair with    
                         | (_,(_,Gray(char))) -> char = targetChar
                         | _ -> false)
        
    let tiles = tupleArray 
                |> Array.map 
                        (fun pair ->  
                            match pair with 
                            | (Gray(char), _) -> 
                                match tryFindExisting char tupleArray with 
                                | Some (index,(left,_)) ->   
                                    tupleArray.[index] <- (left,Yellow(' '))
                                    Yellow(char)
                                | None -> Gray(char)
                            | (l,_) -> l)
                
    tiles

let printTiles (tiles: Tile[] list) = 
    let arr = Array.zeroCreate<string> 5 |> Array.map (fun _ -> "")
    let mutable simple = Table().HeavyBorder().AddColumns(arr)
    simple.ShowHeaders <- false
    let toUpper c = System.Char.ToUpper c

    let prettyString tile = 
        match tile with 
        | Green(c) -> $"[white on green] {toUpper c} [/]"
        | Yellow(c) -> $"[white on darkorange3] {toUpper c} [/]"
        | Gray(c) -> $"[white on gray] {toUpper c} [/]"

    for tile in tiles do    
        let row = tile |> Array.map (fun t -> prettyString t)
        simple = simple.AddRow(row) |> ignore;
    AnsiConsole.Write(simple)

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

let mutable titleFig = FigletText("F# Wordle")
AnsiConsole.Write(titleFig);

let n = Random().Next(wordList.Length)
main wordList.[n] List.empty validWords |> ignore