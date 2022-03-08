// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


open System


module Board = 
   type Cell = Free | X | O
   type Board = Cell list 
   type Index = int*int

   let intializeBoard () : Board =
      let init = [for i in 1..9 do Free] in init:Board

   let updateBoard (board:Board) ((m,n):Index) (newCell:Cell) : board = 
      board |> List.mapi (fun i x -> match i with | m*3+n -> newCell | _ -> x)       

   let isFull (board:Board) : bool = 
      board |> List.exists (fun c -> c=Free) |> not    

   let getCell (board:Board) (c:Index): Cell =
        let i,j=c 
        board.[i*3+j] 

   let writeBoard (board:Board) : unit = 
      let format : string = function | Free -> " " | X -> "X" -> | O -> "O"
      board |> List.iteri (function 2 | 5 | 8 -> printf "%s\n" (format Board.[i]) | _ -> printf "%s " (format Board.[i]) )
       
module Game = 
   type Player = PlayerX | PlayerO 
   type State = Winner | Ongoing | Unresolved   
   type PlayerPos = {Row:int; Col:int}
    
   let (|ValidInput|_|) (cki:ConsoleKeyInfo) (curPos:Index) = 
      let key = cki.Key 
      if key = ConsoleKey.DownArrow && CurPos.Row=3 then None
      elif key = ConsoleKey.UpArrow && CurPos.Row=1 then None   
      elif key = ConsoleKey.LeftArrow && CurPos.Col=1 then None   
      elif key = ConsoleKey.RightArrow && CurPos.Col=3 then None   
      else Some ValidInput

   let (|ThreeInARow|_|) board = 
      let greatestCell,count = board |> List.countBy id  
      if greatestCell=Free || count<3 then None
      else let threes = [for t in 0..2 do board.[t*3..t*3+2]; [t..3..6+t]] in [0,4,8]::([2,4,6]::threes) //0..2, 036, 3..5, 147, 6..8, 258 
           match greatestCount with 
           | X -> if threes |> List.contains [X;X;X] then Some ThreeInARow 
           | O -> if threes |> List.contains [O;O;O] then Some ThreeInARow
         


   let rec readInput (curPos:Index) (board:Board) (player:Player) : board =
      let cki = Console.ReadKey(true) //true = don't print to console
      match cki.Key with 
      | ConsoleKey.X -> let check = getCell board curPos in if check=Free then updateBoard board curPos X 
                                                            else Console.SetCursorPosition(1,4); printfn "Slot is occupied, look for another slot!"; 
                                                                 Console.SetCursorPosition(1,4); eadInput curPos board player 
      | ConsoleKey.O -> let check = getCell board curPos in if check=Free then updateBoard board curPos O
                                                            else Console.SetCursorPosition(1,4); printfn "Slot is occupied, look for another slot!"; 
                                                                 Console.SetCursorPosition(1,4); eadInput curPos board player 
  
      |_ -> match 

      | ValidInput cki -> match cki.Key with 
                          | ConsoleKey.RightArrow -> changePos 
                          | ConsoleKey.LeftArrow->
                          | ConsoleKey.UpArrow->
                          | ConsoleKey.DownArrow ->
  
   let eval (board:Board) : GameState = 
      match board with 
      | ThreeInArow -> 

   nextTurn (player:Player) (board:Board) : (state:GameState) =
       
      
   take input -> check if someone won and draw on board 
 
   let rec playGame () =
      


[<EntryPoint>]
let main argv =
    playGame
    0 // return an integer exit code
