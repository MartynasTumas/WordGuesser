module Game

open System
open Configuration

let GetWord = 
    let random = Random()
    let index = random.Next(0, WORDS.Length)
    WORDS.[index]

let CheckGuess (word:string) (c:string) =
    if CASE_SENSITIVE then word.Contains(c)
        else word.ToLower().Contains(c.ToLower())

let rec GetHelp(word:List<char>) (guesses:List<string>) =
    match word, guesses with
    |h::t, _ when not(CheckGuess (System.String.Concat(Array.ofList(guesses))) (h.ToString())) -> h
    |h::t, _ when CheckGuess (System.String.Concat(Array.ofList(guesses))) (h.ToString()) -> GetHelp t guesses
    |h::t,[] -> h
    |h::_,_ -> h
    |_ -> raise (new InvalidOperationException("Can't get help"))

let rec ReadInput(word:List<char>) (guesses:List<string>) =
    let key = Console.ReadKey()
    if 
        key.Modifiers = ConsoleModifiers.Control && key.Key = ConsoleKey.H && HELP
    then 
        printf"HELP USED"; [GetHelp word guesses]
    elif 
        key.Key = ConsoleKey.Enter then []
    elif 
        key.KeyChar |> Char.IsLetterOrDigit then key.KeyChar :: ReadInput word guesses
    elif 
        key.KeyChar |> Char.IsWhiteSpace then key.KeyChar :: ReadInput word guesses
    else
        ReadInput word guesses


let rec CorrectOrder (word:List<char>) (guesses:string) =
    match word with
    |h::t when ALLOW_BLANKS && h=' ' -> [' ']@ CorrectOrder t guesses
    |h::t when CheckGuess guesses (h.ToString()) -> [h]@ CorrectOrder t guesses
    |h::t when not(CheckGuess guesses (h.ToString())) -> [Char.Parse(HIDDEN)]@ CorrectOrder t guesses
    |_ -> []
let rec GetCorrectGuesses (word:string) (guesses:List<string>) =
    match guesses with
    |h::t when CheckGuess word h -> [h]@ GetCorrectGuesses word t
    |h::t when not(CheckGuess word h) -> [HIDDEN]@ GetCorrectGuesses word t
    |_ -> []
    
let tolow (c:char) = c |> Char.ToLower

let PlayGame () = 
    printfn "Welcome to Word Guesser"
    printfn ""
    printfn "To use help press Ctrl+h"
    printfn ""
    let mutable playTurn = true
    let word = GetWord;
    let wordList = Seq.toList word;
    let mutable guesses = []

    printfn "The lenght of word: %i" wordList.Length
    printfn ""
    while playTurn do      
        printfn "Waiting for a guess..."
        let input = ReadInput wordList guesses;

        if(input.Length=1) then //if input is one character
            if(List.contains (input.[0].ToString()) guesses = false)
                then
                    guesses <- input.[0].ToString() :: guesses
        elif(input.Length>1 && MULTIPLE) then //if input is multiple characters
            if(List.contains (System.String.Concat(Array.ofList(input))) guesses = false)
                then
                    guesses <- (System.String.Concat(Array.ofList(input))) :: guesses
        else guesses <- guesses       

        let guessedWord = GetCorrectGuesses word guesses |> String.Concat 
        let displayWord = CorrectOrder wordList guessedWord |> String.Concat

        if(displayWord.Contains(HIDDEN))
        then
            printfn ""
            printfn "%s" displayWord
        else
            printfn ""
            printfn "%s" displayWord
            printfn "You won. Used guesses: %i" guesses.Length
            printfn "Press enter to exit"
            Console.ReadLine()
            playTurn <- false
