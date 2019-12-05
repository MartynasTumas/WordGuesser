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
    |h::t, _ when not(CheckGuess (String.Concat(Array.ofList(guesses))) (h.ToString())) -> h
    |h::t, _ when CheckGuess (String.Concat(Array.ofList(guesses))) (h.ToString()) -> GetHelp t guesses
    |h::t,[] -> h
    |h::_,_ -> h
    |_ -> raise (new InvalidOperationException("Can't get help"))

let rec ReadInput(word:List<char>) (guesses:List<string>) =
    let key = Console.ReadKey()
    if key.Modifiers = ConsoleModifiers.Control && key.Key = ConsoleKey.H && HELP
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
    |h::t when not(CheckGuess word h) -> GetCorrectGuesses word t
    |_ -> []
   
let PlayGame () = 
    printfn "Welcome to Word Guesser"
    printfn ""
    printfn "To use help press Ctrl+h. Help is enabled: %b" HELP
    printfn ""
    let mutable playTurn = true // Boolian to determine when game should continue and when stop.
    let word = GetWord; // Word(string) to guess which is randomly got from an array of words.
    let wordList = Seq.toList word; // Char list of word characters.
    let mutable guesses = [] // String list of guesses. It also includes guess when help is called.

    printfn "The lenght of word: %i" wordList.Length
    printfn ""
    while playTurn do      // Main game loop that iterates each time user provides character.
        printfn "Waiting for a guess..."
        let input = ReadInput wordList guesses; // User input which is got from ReadInput function. Input is char array.

        if(input.Length=1) then // if input is one character.
            if(List.contains (input.[0].ToString()) guesses = false) // if same guess is guessed multiple times, it doesn't add to the guesses list,
                                                                    // so same guess multiple times will count as an one guess.
                then
                    guesses <- input.[0].ToString() :: guesses // Add guess to the list.
        elif(input.Length>1 && MULTIPLE) then // If input contains multiple characters and program is set to allow multiple characters.
            if(List.contains (String.Concat(Array.ofList(input))) guesses = false)// Here it also checks if same guess already exists.
                                                        // Input is an char array of characters, so 
                                                        // before checking if guess already exists, it needs to add all characters in one string.
                then
                    guesses <- (String.Concat(Array.ofList(input))) :: guesses//Add guess to the list.

        let guessedWord = GetCorrectGuesses word guesses |> String.Concat // Returns a string of characters that are in guesses list AND in a word.
        let displayWord = CorrectOrder wordList guessedWord |> String.Concat // Returns guessedWord string in same order as an word that needs to be guessed.
                                                                    // If guessedWord doesn't contain character(s) that is in the acutal word, it will be raplaced with hidden character.

        if(displayWord.Contains(HIDDEN))// If displayWord contains hidden characters, program will print word and start loop again.
        then
            printfn ""
            printfn "%s" displayWord
        else// If there is no more hidden characters in the word it means that the word was guessed and game is finished.
            printfn ""
            printfn "%s" displayWord
            printfn "You won. Used guesses: %i" guesses.Length// Print how many times user guessed
            printfn "Press enter to exit"
            Console.ReadLine()
            playTurn <- false // Stop loop when word is guessed.
