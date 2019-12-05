module Game

open System
open Configuration

let GetWord = 
    let random = Random()
    let index = random.Next(0, WORDS.Length)
    WORDS.[index]

exception OuterError of string
let rec GetHelp(word:List<char>) (characters:List<string>) =
    match word, characters with
    |h::t, _ when not(List.contains (h.ToString()) characters) -> h
    |h::t, _ when List.contains (h.ToString()) characters -> GetHelp t characters
    |h::t,[] -> h
    |h::_,_ -> h
    |_ -> raise (OuterError("outer"))

let rec ReadInput(word:List<char>) (characters:List<string>) =
    let key = Console.ReadKey()
    if 
        key.Modifiers = ConsoleModifiers.Control && key.Key = ConsoleKey.H && HELP
    then 
        [GetHelp word characters]
    elif 
        key.Key = ConsoleKey.Enter then []
    elif 
        key.KeyChar |> Char.IsLetterOrDigit then key.KeyChar :: ReadInput word characters
    elif 
        key.KeyChar |> Char.IsWhiteSpace then key.KeyChar :: ReadInput word characters
    else
        ReadInput word characters


let CheckGuess (word:string) (c:string) =
    if CASE_SENSITIVE then word.Contains(c)
    else word.ToLower().Contains(c.ToLower())


let rec CorrectOrder (word:List<char>) (guesses:string) =
    match word with
    |h::t when ALLOW_BLANKS && h=' ' -> [' ']@ CorrectOrder t guesses
    |h::t when guesses.Contains(h) -> [h]@ CorrectOrder t guesses
    |h::t when not(guesses.Contains(h)) -> [Char.Parse(HIDDEN)]@ CorrectOrder t guesses
    |_ -> []

let rec GetCorrectGuesses (word:string) (guesses:List<string>) =
    match guesses with
    |h::t when CheckGuess word h -> [h]@ GetCorrectGuesses word t
    |h::t when not(CheckGuess word h) -> [HIDDEN]@ GetCorrectGuesses word t
    |_ -> []
    
let tolow (c:char) = c |> Char.ToLower

let PlayGame () = 
    printfn "Welcome to Word Guesser"
    let mutable playTurn = true
    let mutable word = GetWord;
    let mutable wordList = Seq.toList word;

    let mutable guesses = []
    printfn "The lenght of word: %i" wordList.Length

    while playTurn do      
        printfn "Waiting for a guess..."
        let input = ReadInput wordList guesses;

        if(input.Length=1) then
            if(List.contains (input.[0].ToString()) guesses = false)
                then
                    guesses <- input.[0].ToString() :: guesses
        elif(input.Length>1 && MULTIPLE) then
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
