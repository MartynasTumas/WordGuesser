module Game

open System
open Configuration

(*
let rec CheckCharacter (guessedCharacters:List<char>) (wordCharacter:char) =
    match guessedCharacters with
    |h::t when h=wordCharacter -> true
    |h::t when h<>wordCharacter -> CheckCharacter t wordCharacter
    | _ -> false
    
let normalize c =
    if CASE_SENSITIVE then c
    else Char.ToLowerInvariant c



let rec CheckWord (word:List<char>) (characters:List<char>) =
    match word, characters with
    |h::t, _ when ALLOW_BLANKS && h=' ' -> ['_']@ CheckWord t characters
    |h::t, _ when CheckCharacter characters (normalize(h)) -> [h]@ CheckWord t characters
    |h::t, _ when not(CheckCharacter characters (normalize(h))) -> [HIDDEN]@ CheckWord t characters
    | _ -> []
    *)

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


let rec fixPlacement (word:List<char>) (guesses:string) =
    match word with
    |h::t when ALLOW_BLANKS && h=' ' -> [' ']@ fixPlacement t guesses
    |h::t when guesses.Contains(h) -> [h]@ fixPlacement t guesses
    |h::t when not(guesses.Contains(h)) -> [Char.Parse(HIDDEN)]@ fixPlacement t guesses
    |_ -> []



let rec CheckGuesses (word:string) (guesses:List<string>) =
    match guesses with
    |h::t when word.Contains(h) -> [h]@ CheckGuesses word t
    |h::t when not(word.Contains(h)) -> [HIDDEN]@ CheckGuesses word t
    |_ -> []
    
let tolow (c:char) = c |> Char.ToLower

let PlayGame () = 
    printfn "Welcome to Word Guesser"
    let mutable playTurn = true
    let mutable word = GetWord;
    let mutable wordList = Seq.toList word;  //['H'; 'O'; 'M'; 'E']

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
            if(List.contains (input.[0].ToString()) guesses = false)
            then
                guesses <- (System.String.Concat(Array.ofList(input))) :: guesses
        else guesses <- guesses       
        
        if not CASE_SENSITIVE 
            then 
                word <- word.ToLower();
                wordList <- (wordList |> List.map tolow)
                guesses <- (guesses |> List.map (fun c -> c.ToLower()))


        let guessedWord = CheckGuesses word guesses |> String.Concat 
        let displayWord = fixPlacement wordList guessedWord |> String.Concat
       // let guessedWord = CheckWord wordList guesses
       // let displayWord = guessedWord |> String.Concat 

        if(displayWord.Contains(HIDDEN))
        then
            printfn ""
            printfn "%s" displayWord
        else
            printfn ""
            printfn "%s" displayWord
            printfn "You won. Used guesses %i" guesses.Length
            playTurn <- false
